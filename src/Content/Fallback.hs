module Content.Fallback
  ( preferredLangOrder,
  )
where

import Config (Language (..), SiteConfig (..))
import Content.Types (LangCode)

-- | Language preference order for fallback resolution.
-- The requested language is first; all other configured languages follow in
-- their configured order. This lets callers iterate until they find content.
preferredLangOrder :: SiteConfig -> LangCode -> [LangCode]
preferredLangOrder cfg requested = requested : filter (/= requested) (map langCode $ languages cfg)
