-- To run install ghc/ghci
-- Run command: ghci 01_public_transport.hs
-- You can test functions by using testDB and other methods

import qualified Data.List as D

type TripId    = Integer
data Transport = Plane | Train | Bus | Ship deriving (Eq, Show)
type Place     = String
type Price     = Integer
type Trip      = (TripId, Transport, Place, Place, Price)

type TicketId = Integer
type PersonId = Integer
data Tarif    = Adult | Child | Student | Senior deriving (Eq, Show)
type Ticket   = (TicketId, PersonId, TripId, Tarif)

type Database = ([Ticket], [Trip])


-- Getters
getTripId :: Trip -> TripId
getTripId (x, _, _, _, _) = x

getTransport :: Trip -> Transport
getTransport (_, x, _, _, _) = x

getPlaceFrom :: Trip -> Place
getPlaceFrom (_, _, x, _, _) = x

getPlaceTo :: Trip -> Place
getPlaceTo (_, _, _, x, _) = x

getPrice :: Trip -> Price
getPrice (_, _, _, _, x) = x

getTicketId :: Ticket -> TicketId
getTicketId (x, _, _, _) = x

getPersonId :: Ticket -> PersonId
getPersonId (_, x, _, _) = x

getTicketTripId :: Ticket -> TripId
getTicketTripId (_, _, x, _) = x

getTarif :: Ticket -> Tarif
getTarif (_, _, _, x) = x

lookupTarif :: TicketId -> [Ticket] -> Tarif
lookupTarif id (x:xs) = if getTicketId x == id
                       then getTarif x
                       else lookupTarif id xs

findStops :: (Trip -> Place) -> Place -> Trip -> Bool
findStops op stop x = stop == op x

tripsFrom :: Place -> [Trip] -> [Trip]
tripsFrom stop = filter (findStops getPlaceFrom stop)

tripsTo :: Place -> [Trip] -> [Trip]
tripsTo stop = filter (findStops getPlaceTo stop)

transports :: Place -> Place -> [Trip] -> [Transport]
transports a b x = D.nub $ map getTransport $ (tripsFrom a) $ (tripsTo b) x

updatePrice :: TripId -> Price -> [Trip] -> [Trip]
updatePrice tid new x = map f x
    where f :: Trip -> Trip
          f (tripid, a, b, c, price) = if tripid == tid
                                       then (tripid, a, b, c, new) 
                                       else (tripid, a, b, c, price)

tripCountByTransport :: Transport -> [Trip] -> Int
tripCountByTransport _ [] = 0
tripCountByTransport trans (x:xs) = if trans == getTransport x
                                    then 1 + tripCountByTransport trans xs
                                    else tripCountByTransport trans xs

travelsBy :: PersonId -> TripId -> [Ticket] -> Bool
travelsBy pid tid x = any f x
    where f :: Ticket -> Bool
          f (_, perid, tripid, _) = pid == perid && tid == tripid

peopleByTarif :: Tarif -> [Ticket] -> [PersonId]
peopleByTarif tar x = 
    let f1 a = getTarif a == tar
        f2 b = getPersonId b
    in D.nub $ map f2 $ filter f1 x


travellersFrom :: Place -> Database -> [PersonId]
travellersFrom stop (tick, trip) = D.nub $ goThough (tripIdsFrom stop trip) (makeTuples tick)
    where 
        goThough :: [TripId] -> [(TripId, PersonId)] -> [PersonId]
        goThough [] _ = []
        goThough (x:xs) tup = checkTuples x tup ++ goThough xs tup 

        checkTuples :: TripId -> [(TripId, PersonId)] -> [PersonId]
        checkTuples _ [] = []
        checkTuples x ((y,per):ys) = if x == y
                             then per : checkTuples x ys
                             else checkTuples x ys

        makeTuples :: [Ticket] -> [(TripId, PersonId)]
        makeTuples x = map (\s -> (getTicketTripId s, getPersonId s)) x

        tripIdsFrom :: Place -> [Trip] -> [TripId]
        tripIdsFrom stop x = map getTripId $ filter (\ x -> stop == getPlaceFrom x) x


ticketPrice :: (Tarif -> Price -> Price) -> TripId -> Tarif -> [Trip] -> Price
ticketPrice op tripid tarif x = op tarif (getPrice $ getOnly $ filter (\ s -> tripid == getTripId s) x)
    where
        getOnly :: [Trip] -> Trip
        getOnly (x:_) = x

spent :: (Tarif -> Price -> Price) -> PersonId -> Database -> Price
spent op perid (tick, trip) = sum $ map makePrices $ triTuples (makeTuples1 tick) (makeTuples2 trip) 
    where
        makeTuples1 :: [Ticket] -> [(TripId, Tarif)]
        makeTuples1 x = map (\s -> (getTicketTripId s, getTarif s)) $ filter (\a -> getPersonId a == perid) x

        makeTuples2 :: [Trip] -> [(TripId, Price)]
        makeTuples2 y = map (\s -> (getTripId s, getPrice s)) y

        triTuples :: [(TripId, Tarif)] -> [(TripId, Price)] -> [(TripId, Tarif, Price)]
        triTuples [] _ = []
        triTuples (x:xs) tups = concatTuples x tups ++ triTuples xs tups

        concatTuples :: (TripId, Tarif) -> [(TripId, Price)] -> [(TripId, Tarif, Price)]
        concatTuples _ [] = []
        concatTuples (a, b) ((c, d):xs) = if a == c
                                          then (a, b, d) : concatTuples (a, b) xs
                                          else concatTuples (a, b) xs

        makePrices :: (TripId, Tarif, Price) -> Price
        makePrices (_, b, c) = op b c


-------------------------------------------------------------------------------
--                     S A M P L E    D A T A B A S E                        --
-------------------------------------------------------------------------------

testDB :: Database
testDB = (tickets, trips)
  where
    tickets = [ (0, 42, 0, Student), (1, 42, 4, Student)
              , (5, 4, 2, Adult), (42, 142, 5, Child), (0, 42, 2, Student)
              ]
    trips = [ (0, Plane, "Brno", "Praha", 1000), (1, Plane, "Praha", "Brno", 1050)
            , (2, Train, "Brno", "Praha", 400), (3, Train, "Praha", "Brno", 450)
            , (4, Bus, "Bratislava", "Kosice", 351), (5, Ship, "Bergen", "Stavanger", 5000)
            ]

tarifSR :: Tarif -> Price -> Price
tarifSR Adult price = price
tarifSR _     _     = 0

tarifCR :: Tarif -> Price -> Price
tarifCR Adult price = price
tarifCR Child _     = 0
tarifCR _     price = div (price * 3) 4

tarifNoDiscount :: Tarif -> Price -> Price
tarifNoDiscount _ p = p

freeChild :: Tarif -> Price -> Price
freeChild Child _ = 0
freeChild _ price = price

tripDB :: [Trip] 
tripDB = [(0, Plane, "Brno", "Praha", 1000), (1, Plane, "Praha", "Brno", 1050), (2, Train, "Brno", "Praha", 400), (3, Train, "Praha", "Brno", 450), (4, Bus, "Bratislava", "Kosice", 351), (5, Ship, "Bergen", "Stavanger", 5000)]
tickDB :: [Ticket]
tickDB = [(0, 42, 0, Student), (1, 42, 4, Student), (5, 4, 2, Adult), (42, 142, 5, Child)]
