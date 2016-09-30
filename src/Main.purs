module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe
import DOM (DOM())
import React
import React.DOM as D
import React.DOM.Props as P

import CNF

main :: forall eff. Eff (console :: CONSOLE, dom :: DOM | eff) Unit
main = do
  log "Starting program"
  doc <- window >>= document
  elem <- getElementById (ElementId "react") (documentToNonElementParentNode (htmlDocumentToDocument doc))
  c <- unsafePartial fromJust (toMaybe elem)
  render (D.text "Hello!") c

newtype State = State 
    { formula :: Maybe Formula
    , showCnf :: Boolean
    , showValue :: Boolean
    }

initialState :: State
initialState = State
    { formula = Nothing
    , showCnf = false
    , showValue = false
    }

cnfPractice :: forall props. ReactClass props
cnfPractice = createClass $ spec initialState \ctx -> do
    props <- getProps ctx
    D.div [ P.className "container" ] $
        case state.formula of
            Nothing -> [newFormulaButton]
            Just formula ->
                [ renderFormula formula
                , newFormulaButton
                , showCnfButton
                , showValueButton
                ]
    where
        newFormulaButton = button "New formula"
        showCnfButton = button "Show CNF"
        showValueButton = button "Show Value"


button text =
    createElement
        (createClassStateless (\props -> D.button [ D.text text ]))
        []

