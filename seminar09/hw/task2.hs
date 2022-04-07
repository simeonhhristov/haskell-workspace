main :: IO()
main = do

    print $ coldestCapital [(Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)]), (Country "Germany" "Berlin" [(City "Munchen" 200 15), (City "Berlin" 150 12), (City "Ulm" 210 15)]), (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] --  == "Germany"
    --print $ getLowestTemp [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)]

type Name = String
type Capital = Name
type AvgYearlyTemperature = Double
type Elevation = Int
data City = City Name Elevation AvgYearlyTemperature
data Country = Country Name Capital [City]

getLowestTemp :: [City] -> (Name, AvgYearlyTemperature)
getLowestTemp list = foldr1 (\x@(name1, temp1) y@(name2, temp2) -> if temp1 > temp2 then y else x ) [(name, temperature) | (City name elevation temperature) <- list]


coldestCapital :: [Country] -> Name
coldestCapital list = fst $ foldr1 (\ x@(name1, (cityName1,temp1)) y@(name2, (cityName2, temp2)) -> if temp1 > temp2 then y else x)  [(name, getLowestTemp cities) | (Country name capital cities) <- list]

