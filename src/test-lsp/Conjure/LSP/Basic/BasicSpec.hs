module Conjure.LSP.Basic.BasicSpec where
import Conjure.Prelude
import Test.Hspec
import Conjure.LSP.Util (withDummyServer, doWithSession)
spec :: Spec
spec = around withDummyServer $ do 
    describe "Basic connection" $ do
        it "connects" $ doWithSession $ do 
            return ()


    

