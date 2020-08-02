module Mock.Mock
    ( runView
    ) where

import qualified Model.Core.Types     as T
import qualified Data.Text            as Tx
import           Data.Text                  ( Text           )
import           Data.Monoid                ( appEndo        )
import           Control.Monad.Reader       ( runReader      )
import           Control.Monad.Writer       ( execWriterT    )

runView :: T.ViewMonad a -> Text
runView = Tx.concat
          . flip appEndo []
          . flip runReader ( T.defaultConfig { T.cSortJSets = False } )
          . execWriterT
