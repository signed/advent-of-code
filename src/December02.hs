module December02 where

type Row = [Int]
type SpreadSheet = [Row]

spreadsheetChecksum :: SpreadSheet -> Int
spreadsheetChecksum spreadsheet = sum $ map minMaxDifference spreadsheet
                        where
                          minMaxDifference :: Row -> Int
                          minMaxDifference row = maximum row - minimum row



