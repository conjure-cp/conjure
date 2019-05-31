{-# LANGUAGE QuasiQuotes #-}
module Conjure.Rules.Vertical.Functional.Transform where
import Conjure.Rules.Vertical.Variant (onTagged)
import Conjure.Rules.Import


rule_Transform_Variant_Eq_Right :: Rule
rule_Transform_Variant_Eq_Right = "transform-variant-eq-right" `namedRule` theRule where
  theRule p = do
    (x,t) <- match opEq p
    (morphism, y) <- match opTransform t
    inn <- morphing =<< typeOf morphism    
    TypeVariant{} <- typeOf x
    ty@(TypeVariant{}) <- typeOf y
    (xWhich:xs)   <- downX1 x
    (yWhich:ys)   <- downX1 y
    let maptrans z = [essence| transform(&morphism, &z) |]
    if let ?typeCheckerMode = StronglyTyped in ty `containsType` inn
      then return
            ( "Vertical rule for right transformed variant equality"
            , return $ make opAnd $ fromList
                [ [essence| &xWhich = &yWhich |]
                , onTagged (make opEq) xWhich xs (maptrans <$> ys)
                ]
            )
      else return
            ( "Vertical rule for right transformed variant equality"
            , return [essence| &x = &y |] 
            )
  
rule_Transform_Variant_Eq_Left :: Rule
rule_Transform_Variant_Eq_Left = "transform-variant-eq-left" `namedRule` theRule where
  theRule p = do
    (t,y) <- match opEq p
    (morphism, x) <- match opTransform t
    inn <- morphing =<< typeOf morphism    
    tx@(TypeVariant{}) <- typeOf x
    TypeVariant{} <- typeOf y
    (xWhich:xs)   <- downX1 x
    (yWhich:ys)   <- downX1 y
    let maptrans z = [essence| transform(&morphism, &z) |]
    if let ?typeCheckerMode = StronglyTyped in tx `containsType` inn
      then return
            ( "Vertical rule for right transformed variant equality"
            , return $ make opAnd $ fromList
                [ [essence| &xWhich = &yWhich |]
                , onTagged (make opEq) xWhich (maptrans <$> xs) ys 
                ]
            )
      else return
            ( "Vertical rule for right transformed variant equality"
            , return [essence| &x = &y |] 
            )
 
 
