

main :: IO ()
-- main = interact (unwords . map (show . length) . words)

main = interact go
    where
        go str =
            let tokens = words str
                lengths = map length tokens
                result = unwords (map show lengths)
            in result
