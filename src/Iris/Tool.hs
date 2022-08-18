{- |
Module                  : Iris.Tool
Copyright               : (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

Utilities to check required tools and their minimal version for a CLI app.

@since 0.0.0.0
-}

module Iris.Tool
    ( -- * Types describing executable requirements
      Tool (..)
    , ToolSelector (..)
    , defaultToolSelector

      -- * Tool requirements check
    , ToolCheckResult (..)
    , checkTool
    ) where

import Data.String (IsString (..))
import Data.Text (Text)
import System.Directory (findExecutable)
import System.Process (readProcess)

import qualified Data.Text as Text


{- | Describes a tool used by your CLI application.
@since 0.0.0.0
-}
data Tool cmd = Tool
    { -- |
      -- | @since 0.0.0.0
      toolName     :: Text

      -- | @since 0.0.0.0
    , toolSelector :: Maybe (ToolSelector cmd)
    }

{- |

@since 0.0.0.0
-}
instance IsString (Tool cmd) where
    fromString :: String -> Tool cmd
    fromString s = Tool
        { toolName     = fromString s
        , toolSelector = Nothing
        }

{- | Describes how to identify if tool is sutable for the application.
  @toolSelectorFunction@ - accepts @cmd@ and tool version and returns @True@ if tool version is acceptable and @False@ otherwise.
  @toolSelectorVersionArg@ - argument for the tool call to get a version.

@since 0.0.0.0
-}
data ToolSelector cmd = ToolSelector
    { -- | @since 0.0.0.0
      toolSelectorFunction   :: cmd -> Text -> Bool

      -- | @since 0.0.0.0
    , toolSelectorVersionArg :: Maybe Text
    }

{- | Tool selector if the version doesn't matter.

@since 0.0.0.0
-}
defaultToolSelector :: ToolSelector cmd
defaultToolSelector = ToolSelector
    { toolSelectorFunction   = \_cmd _version -> True
    , toolSelectorVersionArg = Nothing
    }

{- |

@since 0.0.0.0
-}
data ToolCheckResult
    {- |

    @since 0.0.0.0
    -}
    = ToolNotFound Text

    {- |

    @since 0.0.0.0
    -}
    | ToolWrongVersion Text

    {- |

    @since 0.0.0.0
    -}
    | ToolOk
    deriving stock
        ( Show  -- ^ @since 0.0.0.0
        , Eq    -- ^ @since 0.0.0.0
        )

{- | Checks if tool is ok.
  Returns @ToolNotFound@ if @toolName@ is not found by @findExecutable@.
  Returns @ToolWrongVersion@ if specific tool version is requered by @ToolSelector@ but @findExecutable@ found the wrong version.
  Returns @ToolOk@ otherwise.

@since 0.0.0.0
-}
checkTool :: cmd -> Tool cmd -> IO ToolCheckResult
checkTool cmd Tool{..} = findExecutable (Text.unpack toolName) >>= \case
    Nothing  -> pure $ ToolNotFound toolName
    Just exe -> case toolSelector of
        Nothing               -> pure ToolOk
        Just ToolSelector{..} -> case toolSelectorVersionArg of
            Nothing         -> pure ToolOk
            Just versionArg -> do
                toolVersionOutput <- readProcess exe [Text.unpack versionArg] ""
                let version = Text.strip $ Text.pack toolVersionOutput

                if toolSelectorFunction cmd version
                then pure ToolOk
                else pure $ ToolWrongVersion version
