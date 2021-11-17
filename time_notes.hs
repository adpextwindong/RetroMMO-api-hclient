--
--Notes on the timeformat in the json, we'll need to sort this out when we actually parse dates
--
import Data.Time (iso8601DateFormat, parseTimeOrError, defaultTimeLocale)
import Data.Time.Clock (UTCTime)

tx = "2020-08-14T03:15:17.000Z"
ty :: UTCTime
format = (iso8601DateFormat (Just "%H:%M:%S")) <> ".000Z" --Not sure why .000Z is tagged on but this works
ty = parseTimeOrError True defaultTimeLocale format tx
