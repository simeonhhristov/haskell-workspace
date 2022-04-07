main :: IO()
main = do
    print $ highestCapital [(Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)]), (Country "Germany" "Berlin" [(City "Munchen" 200 15), (City "Berlin" 150 12), (City "Ulm" 210 15)]), (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] == "Bulgaria"

type Name = String
type Capital = Name
type AvgYearlyTemperature = Double
type Elevation = Int
data City = City Name Elevation AvgYearlyTemperature
 deriving (Show)
data Country = Country Name Capital [City]

getCapital :: Name -> [City] -> (Name, Elevation)
getCapital capital list = head [ (name, elevation) | (City name elevation temperatue) <- list, name == capital]


highestCapital :: [Country] -> Name
highestCapital list = fst $ foldr1 (\x@(country1, (cap1, elev1)) y@(country2, (cap2, elev2)) -> if elev1 > elev2 then x else y)  [(name, (getCapital capital cities)) | (Country name capital cities) <-list]

