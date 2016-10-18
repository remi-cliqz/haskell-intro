

len (_:xs) = 1 + len xs

main :: IO ()
main = print (len [1..20])
