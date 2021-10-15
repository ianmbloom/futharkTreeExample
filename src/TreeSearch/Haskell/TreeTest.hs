{-# LANGUAGE PatternSynonyms #-}

module TreeSearch.Haskell.TreeTest

where

import Data.Massiv.Array (size, pattern Sz2)

import TreeSearch.Debug
import TreeSearch.Haskell.Type
import TreeSearch.Haskell.Util
import TreeSearch.Haskell.Pixel
import TreeSearch.Haskell.Image
import TreeSearch.Haskell.Hoas
import TreeSearch.Haskell.Tree.Tesselate
import TreeSearch.Haskell.Tree.Build
import TreeSearch.Haskell.Tree.Verify
import TreeSearch.Haskell.RectNode
import TreeSearch.Haskell.RangeNode
import TreeSearch.MassivGrid

adjustedFrame :: Array2d Pixel -> Array2d UvnRange
adjustedFrame frame =
    let (Sz2 h w) = size frame
        lgSize = max (adjustedLog h) (adjustedLog w)
        lgRect = LgRect { lgH = Y lgSize
                        , lgW = X lgSize
                        }
        dh = powerOf2 (unY $ lgH lgRect)
        dw = powerOf2 (unX $ lgW lgRect)
        rangeFrame = map2d pixelToNode frame
        adjFrame = reframeImage dh dw nullUvnRange rangeFrame
    in  adjFrame

verifyUvImage :: Array2d Pixel -> (Array2d Int, Array2d Pixel)
verifyUvImage frame =
    let (Sz2 h w) = size frame
        lgSize = max (adjustedLog h) (adjustedLog w)
        lgRect = LgRect { lgH = Y lgSize
                        , lgW = X lgSize
                        }
        adjFrame = adjustedFrame frame
        verifiedFrame = verifySearch combineUvnRange
                                     canContainUvnRange
                                     chooseUvnRange
                                     nullUvnRange
                                     stopUvnRange
                                     lgRect
                                     adjFrame
        boolFrame = map2d verifiedFound verifiedFrame
        itemFrame = map2d verifiedItem  verifiedFrame
        posFrame  = map2d verifiedPos   verifiedFrame
    in  ( map2d boolToInt boolFrame
        , map2d nodeToPixel itemFrame
        )

verifyUvFrame :: Int -> Int -> (Array2d Int, Array2d Pixel)
verifyUvFrame lgHeight lgWidth =
    let frame = gradientImage (powerOf2 lgHeight) (powerOf2 lgWidth)
    in  verifyUvImage frame

testTreeNode :: Array2d Pixel -> [UvnRange]
testTreeNode frame =
    let (Sz2 h w) = size frame
        lgSize = max (adjustedLog h) (adjustedLog w)
        lgRect = LgRect { lgH = Y lgSize
                        , lgW = X lgSize
                        }
        adjFrame = adjustedFrame frame
        tree = buildTree combineUvnRange
                         lgRect
                         adjFrame
    in  tree

testNodeBox :: Int -> Int -> [[Int]]
testNodeBox lgHeight lgWidth =
    let lgRect = LgRect { lgH = Y lgHeight
                        , lgW = X lgWidth
                        }
        frame = map2d wrapPoint (pointFrame (powerOf2 lgHeight) (powerOf2 lgWidth))
        tree  = buildTree combineRect
                          lgRect
                          frame
    in  map rectToArray tree

verifyRectFrame :: Int -> Int -> (Array2d Int, Array2d Point)
verifyRectFrame lgHeight lgWidth =
    let lgRect = LgRect { lgH = Y lgHeight
                        , lgW = X lgWidth}
        frame = pointFrame (powerOf2 lgHeight) (powerOf2 lgWidth)
        rectFrame = map2d wrapPoint frame
        verifiedFrame = verifySearch combineRect
                                     canContainRect
                                     chooseRect
                                     nullRect
                                     stopRect
                                     lgRect
                                     rectFrame
        boolFrame = map2d verifiedFound verifiedFrame
        itemFrame = map2d verifiedItem  verifiedFrame
    in  ( map2d boolToInt boolFrame
        , map2d stripPoint itemFrame
        )

testTreeBox :: Int -> Int -> [Rect]
testTreeBox lgHeight lgWidth =
    let lgRect = LgRect {lgH = Y lgHeight, lgW = X lgWidth}
        frame  = map2d wrapPoint (pointFrame (powerOf2 lgHeight) (powerOf2 lgWidth))
        tree   = buildTree combineRect
                           lgRect
                           frame
    in  tree
