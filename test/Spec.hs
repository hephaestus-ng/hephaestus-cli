import Data.Tree

import Data.FM.FeatureModel
import Data.FM.Feature
import Data.FM.ProductConfiguration
import Data.FM.Expression
import Data.FM.Tree

import Data.SourceCode

import Data.SPL



main :: IO ()
main = putStrLn "Test suite not yet implemented"


ft03 = Node (Feature "core" BasicFeature Mandatory) [
             (Node (Feature "print" BasicFeature Mandatory) []),
             (Node (Feature "eval" BasicFeature Optional) []),
             (Node (Feature "expr" BasicFeature Mandatory) [
                (Node (Feature "literal" BasicFeature Mandatory) []),
                (Node (Feature "add" BasicFeature Optional) []),
                (Node (Feature "sub" BasicFeature Optional) [])
             ])
            ]


featureModel = FeatureModel ft03 []

pc01 = ["core", "print", "expr", "literal"]

pc02 = "eval" : pc01


ck :: ConfigurationKnowledge ComponentModel
ck = [ (Ref "core", [selectComponent "Expression.java", selectComponent "Literal.java"])
     , (Ref "eval", [selectComponent "Eval.aj"])
     , (Ref "add" , [selectComponent "Add.java"])
     , (And (Ref "add") (Ref "eval"), [selectComponent "AddEval.aj"])
 ]

-- spl :: SPL ComponentModel
spl = SPL featureModel ck

p1 = build spl pc01

p2 = build spl pc02
