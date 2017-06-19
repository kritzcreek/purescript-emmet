module Emmet.Parser.Data where

import Text.Parsing.Parser (Parser)

type EmmetParser a = Parser String a
