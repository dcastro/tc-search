module Loggers where

import Prelude
import Control.Logger (Logger(Logger))
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW, now)
import Data.DateTime.Instant (toDateTime)
import Data.Formatter.DateTime (Formatter, FormatterF(..), format)
import Data.Functor.Mu (roll)
import Node.Encoding (Encoding(..))
import Node.Express.Types (Path)
import Node.FS (FS)
import Node.FS.Sync (appendTextFile)

formatter ∷ Formatter
formatter =
  roll $ Placeholder "["
  $ roll $ YearFull
  $ roll $ Placeholder "-"
  $ roll $ MonthTwoDigits
  $ roll $ Placeholder "-"
  $ roll $ DayOfMonth
  $ roll $ Placeholder " "
  $ roll $ Hours24
  $ roll $ Placeholder ":"
  $ roll $ Minutes
  $ roll $ Placeholder ":"
  $ roll $ Seconds
  $ roll $ Placeholder "."
  $ roll $ Milliseconds
  $ roll $ Placeholder "]"
  $ roll End

timestamp :: forall e m. (MonadEff (now :: NOW | e) m) => m String
timestamp = liftEff now <#> toDateTime >>> format formatter

formatMsg :: forall e m. (MonadEff (now :: NOW | e) m) => String -> m String
formatMsg msg = timestamp <#> (_ <> " " <> msg)


fileLogger :: forall e a m. (Show a, MonadEff (fs :: FS, err :: EXCEPTION, now :: NOW | e) m)
           => Path -> Logger m a
fileLogger path =
  Logger \entry -> entry # show # formatMsg <#> ("\n" <> _) >>= appendTextFile UTF8 path >>> liftEff


consoleLogger :: forall m e a. (Show a, MonadEff (console :: CONSOLE, now :: NOW | e) m)
              => Logger m a
consoleLogger =
  Logger \entry -> entry # show # formatMsg >>= log >>> liftEff
