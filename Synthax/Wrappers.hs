module Synthax.Wrappers
( sourceWrapper
, codeWrapper
, moduleFunctionWrapper
, moduleCodeWrapper
, gainWrapper
, crossfadeWrapper
, filterWrapper
, bufferWrapper
, letWrapper
) where

import Synthax.Builders

import Prelude hiding (max)
import Import hiding (max)
import Data.Array
import Data.Text hiding (foldr)
import Data.Text.Internal.Builder

sourceWrapper :: Builder -> Int -> Builder
sourceWrapper label fileHandle = sid "var " .<> label
    <>. sid " = SourceTool(__audioContext(), __bufferContext.getBuffer("
    <>. show fileHandle <>. sid "), true);\n"

codeWrapper' :: Builder -> Maybe Text -> Text -> Builder
codeWrapper' label mcode function = sid "var " .<> label
    <>. sid " = __audioContext().createScriptProcessor(16384, 0, 1);\n"
    <> label <>. sid ".onaudioprocess = function(audioProcessingEvent) {\n"
    <>. tid (case mcode of
        Nothing -> ""
        Just code -> code)
    <>. sid "\n"
    <>. sid "var outputBuffer = audioProcessingEvent.outputBuffer;\n"
    <>. sid "var currentTime = __audioContext().currentTime;\n"
    <>. sid "for(var channel = 0; channel < outputBuffer.numberOfChannels; channel++) {\n"
    <>. sid "var outData = outputBuffer.getChannelData(channel);\n"
    <>. sid "for(var sample = 0; sample < outputBuffer.length; sample++) {\n"
    <>. sid "var sampleTime = currentTime +\n"
    <>. sid "outputBuffer.duration *\n"
    <>. sid "sample / outputBuffer.length;\n"
    <>. sid "outData[sample] = "
    <>. tid function <>. sid "(sampleTime);\n"
    <>. sid "}\n}\n};\n"

codeWrapper :: Builder -> Text -> Builder
codeWrapper label code = codeWrapper' label (Just code) "gen"

moduleFunctionWrapper :: Builder -> ModuleId -> Builder
moduleFunctionWrapper label mid = codeWrapper' label Nothing
    $ append "__module_" $ toPathPiece mid

moduleCodeWrapper :: ModuleId -> Text -> Builder
moduleCodeWrapper mid code = tid "var __module_"
    .<>. tid (toPathPiece mid)
    <>. sid " = function (t) {\n"
    <>. code <>. sid "\nreturn gen(t);\n}\n"

gainWrapper :: Builder -> Builder -> Double -> Builder
gainWrapper label source value = sid "var " .<> label
    <>. sid " = GainTool(__audioContext(), "
    <> source <>. sid ", " <>. show value <>. sid ");\n"

crossfadeWrapper :: Builder -> Builder -> Builder -> Double -> Double -> Builder
crossfadeWrapper label source source' value max = sid "var " .<> label
    <>. sid " = CrossfadeTool(__audioContext(), "
    <> source <>. sid ", " <> source' <>. sid ", " <>. show value
    <>. sid ", " <>. show max <>. sid ");\n"

filterWrapper :: Builder -> Builder -> Text -> Double -> Builder
filterWrapper label source typ frequency = sid "var " .<> label
    <>. sid " = FilterTool(__audioContext(), "
    <> source <>. sid ", \"" <>. typ <>. sid "\", " <>. show frequency
    <>. sid ");\n"

bufferWrapper :: Int -> Array Int Builder -> Builder
bufferWrapper 0 _ = fromString ""
bufferWrapper size fileArray = let r = if size == 1 then [] else [1..(size - 1)] in
    sid "var __bufferContext = BufferTool(__audioContext(), ["
    .<> foldr addFile (sid "\"" .<> (fileArray ! 0) <>. sid "\"") r
    <>. sid "]);\n"
    where addFile i bs = let f = fileArray ! i in bs <>. sid ", \"" <> f <>. sid "\""

letWrapper :: Builder -> Builder -> Builder
letWrapper label expressionLabel = sid "var " .<> label
    <>. sid " = " <> expressionLabel <>. sid ";\n"

