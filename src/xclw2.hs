{-# LANGUAGE Arrows #-}

import Text.XML.HXT.Core

main = do
	lista <- runX $ readDocument [] "out2.xml" >>> deep (isElem >>> hasName "record") >>> ((getChildren >>> hasName "xclc" >>> (getAttrValue "x" &&& getAttrValue "y")) &&& (getChildren >>> hasName "xclw" >>> getAttrValue "height"))
	mapM_ print lista
