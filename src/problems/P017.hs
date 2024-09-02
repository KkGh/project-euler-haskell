module P017 (main) where

main = do
  let words = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen", "twenty", "thirtyone", "twentytwo", "twentythree", "twentyfour", "twentyfive", "twentysix", "twentyseven", "twentyeight", "twentynine", "thirty", "thirtyone", "thirtytwo", "thirtythree", "thirtyfour", "thirtyfive", "thirtysix", "thirtyseven", "thirtyeight", "thirtynine", "forty", "fortyone", "fortytwo", "fortythree", "fortyfour", "fortyfive", "fortysix", "fortyseven", "fortyeight", "fortynine", "fifty", "fiftyone", "fiftytwo", "fiftythree", "fiftyfour", "fiftyfive", "fiftysix", "fiftyseven", "fiftyeight", "fiftynine", "sixty", "sixtyone", "sixtytwo", "sixtythree", "sixtyfour", "sixtyfive", "sixtysix", "sixtyseven", "sixtyeight", "sixtynine", "seventy", "seventyone", "seventytwo", "seventythree", "seventyfour", "seventyfive", "seventysix", "seventyseven", "seventyeight", "seventynine", "eighty", "eightyone", "eightytwo", "eightythree", "eightyfour", "eightyfive", "eightysix", "eightyseven", "eightyeight", "eightynine", "ninety", "ninetyone", "ninetytwo", "ninetythree", "ninetyfour", "ninetyfive", "ninetysix", "ninetyseven", "ninetyeight", "ninetynine"]
  let toWords 1000 = "onethousand"
      toWords n
        | n >= 100 = (words !! (n `div` 100)) ++ "hundred" ++ (if rest /= "" then "and" ++ rest else "")
        | otherwise = words !! n
        where
          rest = words !! (n `mod` 100)

  print $ sum $ map (length . toWords) [1 .. 1000]
