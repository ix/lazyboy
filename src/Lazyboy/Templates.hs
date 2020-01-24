{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Lazyboy.Templates (templatize, basic) where

import Data.Text         (Text)
import NeatInterpolation (text)

type Template = Text -> Text

templatize :: (Text -> Text) -> Text -> Text
templatize = ($)

basic :: Template
basic body =
  [text|
       ;; DEFINE THE ROM START LOCATION
       SECTION "rom", ROM0
             
       ;; DEFINE THE START LOCATION AS 0x100
       SECTION "start", ROM0[$$0100]
       nop
       jp main
             
       ;; PLACE THE MAIN BLOCK AT 0x150
       SECTION "main", ROM0[$$0150]
             
       main:
       $body
  |]
