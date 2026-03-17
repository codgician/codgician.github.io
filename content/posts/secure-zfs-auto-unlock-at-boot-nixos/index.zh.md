---
title: "在 NixOS 上实现安全的 TPM2 + ZFS 自动解锁"
date: 2026-03-04
language: zh
canonical: false
tags:
  - nixos
  - zfs
  - tpm
  - security
  - encryption
math: false
toc: true
mermaid: true
draft: false
---

用 TPM2 自动解锁磁盘，你肯定希望就算机器被人搬走，数据也还是安全的。但现实很骨感：很多常见配置十分钟就能破，攻击者只需要一把螺丝刀和一个 U 盘。

这篇文章介绍一套为 NixOS 设计的 TPM2 + ZFS 解锁方案，能防住三类常见攻击——而这些恰恰是 Clevis 和基础 systemd-cryptenroll 配置的软肋。

## TPM 平台配置寄存器入门

在讲漏洞之前，先搞清楚 TPM 是怎么保护密钥的。

可信平台模块（TPM）是一块专用安全芯片，能存密钥，而且只在满足特定条件时才把密钥吐出来。核心机制是**平台配置寄存器（PCR）**——24 个特殊寄存器，用来记录启动过程中的度量结果。

### PCR 的工作原理

PCR 里的值不是直接写进去的，而是通过 extend 操作一层层算出来的：

```
新PCR值 = hash(旧PCR值 ‖ 新数据)  # ‖ 表示拼接
```

也就是说：

- 每个 PCR 初始值为零（PCR 17-22 初始值为 0xFF）
- 每次度量都会把旧值和新数据拼起来再做一次哈希
- 这个操作不可逆——哈希是单向的
- 最终值代表了*整条度量链*的结果

### PCR 的分配

[Linux TPM PCR Registry](https://uapi-group.org/specifications/specs/linux_tpm_pcr_registry/) 定义了各 PCR 的标准用途：

| PCR | 名称               | 度量内容                            |
| --- | ------------------ | ----------------------------------- |
| 0   | platform-code      | 核心固件（UEFI）                    |
| 2   | external-code      | 扩展 ROM、外部固件                  |
| 7   | secure-boot-policy | Secure Boot 状态、已注册的证书      |
| 11  | kernel-image       | 内核、initrd、命令行（按 UKI 规范） |
| 15  | system-identity    | 系统身份标识——_后面会细讲_          |

> **注意**：上表只列了几个相关的 PCR，完整规范定义了全部 24 个，详见官方文档。

把密钥封装（seal）到 TPM 时，需要指定一组 PCR 作为策略。攻击者改了 bootloader，PCR 7 就变了，TPM 就不给解封。

听起来挺安全？可惜，还是有漏洞。

## 三类漏洞

### 1. TPM 总线嗅探（CVE-2026-0714）

**问题在于**：很多 TPM 通过未加密的 SPI 或 I2C 总线跟 CPU 通信。能接触到机器的人可以窃听这条总线，把传输中的密钥截走。

[CVE-2026-0714](https://www.cyloq.se/en/research/cve-2026-0714-tpm-sniffing-luks-keys-on-an-embedded-device) 里有完整复现：研究人员用逻辑分析仪嗅探 Moxa 工业计算机的 TPM SPI 总线，开机时一抓就拿到了卷密钥。`TPM2_NV_Read` 命令直接以明文返回——尽管 TPM 正确执行了 PCR 策略检查。

**Clevis 为什么会中招**：Clevis 默认不用 TPM 加密会话。获取封装密钥时，通信在总线上是明文的。攻击者把逻辑分析仪往 TPM 上一接，密钥就到手了。

**怎么防**：用 TPM 加密会话。TPM2 规范支持经过认证和加密的会话，可以防总线级别的窃听。

### 2. 根文件系统混淆攻击

**问题在于**：就算正确绑定了 PCR，大多数方案在执行加密卷里的代码之前，并没有验证这个卷*是不是真的*。

这个攻击手法比较经典，[oddlama 的博客](https://oddlama.org/blog/bypassing-disk-encryption-with-tpm2-unlock/)里有详细描述。流程是这样的：

1. 攻击者从 Live USB 启动，备份你加密分区的头部
2. 创建一个新的加密分区，UUID 相同，但密码攻击者自己定
3. 里面放一个精简的 Linux rootfs，包含恶意的 `/sbin/init`
4. 正常重启机器
5. TPM 解锁失败（分区不对），initrd 回退到密码提示
6. 攻击者输入自己的密码——initrd 挂载了假的根分区
7. 恶意 init 跑起来了，此时 TPM 仍处于有效状态
8. 攻击者的代码向 TPM 请求*真正的*解密密钥——拿到了！

关键在于：启动链中没有任何环节验证加密卷是否合法。initrd 检查了 bootloader 没被改（通过 PCR 7），但没检查加密数据是不是来自正确的卷。

**为什么光靠 PCR 7 不行**：PCR 7 度量的是 Secure Boot 状态和证书——证明的是*代码*没问题，但跟*数据*没关系。攻击者的假卷不会改变任何启动时的 PCR。

**怎么防**：解封之前，用每个加密卷的指纹扩展 PCR 15。这样 TPM 密钥就绑定到了特定的卷，而不只是启动代码。

### 3. 解锁后重放攻击

**问题在于**：磁盘解锁后，TPM 凭据仍然有效。攻击者拿到 root 权限就能重新用。

设想这个场景：

1. 系统正常启动，磁盘通过 TPM 解锁
2. 攻击者利用某个漏洞拿到 root
3. 凭据文件（`.cred`）在磁盘上——root 可读
4. 攻击者用 TPM 解密凭据文件
5. 拿到了磁盘解锁口令

虽然密钥确实会驻留在内核态内存里，但直接从内存捞出来成本更高——现代 Linux 内核有大量保护机制：

- **KASLR**（内核地址空间布局随机化）把内核代码和数据的位置打乱
- **KPTI**（内核页表隔离）分离内核和用户空间的页表
- **CONFIG_HARDENED_USERCOPY** 阻止把内核对象复制到用户空间
- **/dev/mem 和 /dev/kmem 限制**阻止直接访问物理内存
- **lockdown 模式**（启用时）进一步限制内核自省

相比之下，解密凭据文件就简单多了。

**怎么防**：成功解锁后，用固定值扩展 PCR 15。凭据立即失效——就算攻击者拿到 root，TPM 也会拒绝解封，因为 PCR 15 已经跟注册时对不上了。

## 具体实现

本文涉及的 NixOS 系统用的是 ZFS 原生加密而非 LUKS。这个选择有其优点——ZFS 加密与快照、复制和写时复制完美集成——但也意味着不能直接用那些给 LUKS 设计的工具。

### 难点：ZFS + 加密会话

需求：

- TPM 加密会话（防总线嗅探）
- 按预计算值自定义 PCR 绑定（防卷混淆）
- 解锁后 PCR 扩展（防重放）

`systemd-cryptenroll` 提供了加密会话，解决了总线嗅探问题，但只支持 LUKS——不支持 ZFS 原生加密。

`systemd-creds` 底层用的也是同一套 TPM2 逻辑（包括加密会话），但有个硬伤：[没法按指定 PCR 值封装](https://github.com/systemd/systemd/issues/38763)，只能按当前值。也就是说必须在目标 PCR 状态下启动才能注册，预注册就没戏了。

### mkcreds 登场

这个限制催生了 [mkcreds](https://github.com/codgician/mkcreds)——一个用 Claude 辅助写的 Rust 小工具，能创建与 systemd-creds 兼容的凭据，关键是支持**按指定 PCR 值封装**。

```bash
# 按预期的 PCR 15 值封装（预先算好）
echo "secret" | mkcreds --tpm2-pcrs="7+15:sha256=<expected-hex>" - mycred.cred

# 之后正常用 systemd-creds 解密
systemd-creds decrypt mycred.cred -
```

这样就能算出用 ZFS 指纹扩展后 PCR 15 会是什么值，然后针对这个目标状态封装凭据——完全不用重启。

### ZFS 指纹：证明卷的真实性

要防卷混淆，需要一个值满足：

1. 能唯一标识每个加密的 ZFS 数据集
2. 不知道加密密钥就没法伪造

我们从 ZFS 内部的加密元数据派生指纹：

```bash
fingerprint = hash(GUID ‖ MAC)  # ‖ 表示拼接
```

其中：

- **GUID**（`DSL_CRYPTO_GUID`）：加密根的唯一标识符
- **MAC**（`DSL_CRYPTO_MAC`）：密钥加密时产生的 AES-GCM 认证标签

MAC 是关键。AES-GCM 的认证标签取决于明文（密钥）和加密操作本身。攻击者不知道加密密钥，就没法生成有效的 MAC。他们可以创建 GUID 相同的 ZFS 池，但 MAC 肯定对不上。

指纹计算用 `zdb` 直接从 ZFS 元数据提取：

```bash
# zfs-fingerprint 脚本的简化版
crypto_obj=$(zdb -ddddd "$pool" "$root_ds" | grep -oP 'crypto_key_obj = \K\d+')
guid=$(zdb -ddddd "$pool" "$crypto_obj" | grep -oP 'DSL_CRYPTO_GUID = \K\d+')
mac=$(zdb -ddddd "$pool" "$crypto_obj" | grep -oP 'DSL_CRYPTO_MAC = \K[0-9a-f]+')
echo -n "${guid}${mac}" | sha256sum | cut -d' ' -f1
```

### 解锁流程

`zfs-unlock` 模块的安全解锁流程：

```mermaid
flowchart TD
    A["UEFI/Secure Boot"] -->|"PCR 0, 7"| B["Kernel + initrd"]
    B --> C["ZFS pools imported"]
    C --> D["zfs-tpm-unlock.service"]

    subgraph unlock[" "]
        D --> E["Compute fingerprint<br/>hash(GUID ‖ MAC)"]
        E --> F["Extend PCR 15"]
        F --> G{"PCR 15 matches?"}
        G -->|Yes| H["systemd-creds decrypt<br/>(PCR 7 + 15)"]
        G -->|No| FAIL["Unseal fails"]
        H --> I["zfs load-key"]
        I --> J["Extend PCR 15 with zeros<br/>(invalidates credential)"]
    end

    J --> K["Root mounted"]
    FAIL --> L["Password prompt"]
```

### NixOS 集成

模块采用声明式配置：

```nix
{
  codgician.system.zfs-unlock = {
    enable = true;
    devices = {
      "zroot" = {
        credentialFile = ./secrets/zroot.cred;
      };
      "zdata/encrypted" = {
        credentialFile = ./secrets/zdata-encrypted.cred;
      };
    };
  };
}
```

`mkzfscreds` 负责注册：

```bash
# 计算预期的 PCR 15 并创建凭据
nix run .#mkzfscreds -- zroot > hosts/myhost/zroot.cred

# 输出：
# Creating credential for: zroot (host: myhost)
# Devices: zroot
# Computing expected PCR 15...
#   zroot: a3b2c1d0...
# Expected PCR 15: 7f8e9d0c...
# Enter passphrase for zroot:
```

这个工具会自动：

1. 读取当前主机的所有配置设备
2. 计算每个设备的指纹（按排序保证确定性）
3. 模拟 PCR 15 扩展拿到预期值
4. 把凭据封装到 PCR 1、2、7、12、14 和算出来的 15

## 纵深防御

没有哪个单一机制是万无一失的。这套方案层层设防：

| 攻击手法         | 防御措施                        |
| ---------------- | ------------------------------- |
| TPM 总线嗅探     | 加密会话（通过 systemd-creds）  |
| 卷混淆           | 解锁前指纹 → PCR 15             |
| Root 凭据重放    | 解锁后清零 → PCR 15             |
| Bootloader 篡改  | PCR 7（Secure Boot 策略）       |
| 内核/initrd 修改 | Lanzaboote（stub 中的哈希验证） |

就算某一层被突破，其他层依然有效。攻击者需要同时：

1. 绕过加密会话拿到总线明文（需要复杂的硬件攻击）
2. 伪造 ZFS 元数据（需要知道加密密钥）
3. 在反重放扩展之前提取密钥（需要在极短时间窗口内利用内核漏洞）

## 结语

TPM 磁盘解锁听起来很简单——把密钥封装到 PCR，启动时解封。但细节决定成败：

- **加密会话**防止物理总线嗅探
- **卷身份验证**阻止文件系统混淆攻击
- **解锁后失效**限制凭据重放的时间窗口

现有生态存在空白。Clevis 不用加密会话。大多数 systemd-cryptenroll 教程跳过 PCR 15 验证。两者都没很好地支持 ZFS 原生加密。

这套方案——`mkcreds` 创建凭据、ZFS 指纹验证卷身份、精心管理 PCR 15——为使用 ZFS 加密的 NixOS 系统提供了纵深防御。

完整实现在 [serenitea-pot](https://github.com/codgician/serenitea-pot) NixOS 配置的 `modules/nixos/system/zfs-unlock/` 目录。它使用 [Lanzaboote](https://github.com/nix-community/lanzaboote) 实现 Secure Boot——一种无 shim 方案，将内核和 initrd 的 SHA-256 哈希嵌入已签名的 UEFI stub，度量值扩展到 PCR 11。[mkcreds](https://github.com/codgician/mkcreds) 作为独立 Nix flake 提供。

---

_本文描述的漏洞影响着许多实际部署的系统。如果你在用 TPM 磁盘解锁，请仔细审查配置。记住：安全的本质是提高攻击成本，而非追求绝对完美。_
