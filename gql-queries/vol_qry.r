# Task4b

#We need a function that can create from- and to times as strings.
#need a method for converting times in stations_metadata_df into the Vegvesen-format.

library(glue)

#We need a function that creates volume queries similar to the one above, 
# but where station id and from- and to-dates are inserted as arguments.

#Goal:
# Create a function vol_qry(id, from, to)
# this should return a query similar to the one listed above, 
# but where the values of the arguments id, from and to replace 97411V72313, 2022-05-01T06:55:47.172Z and 2022-05-08T06:55:47.172Z.

vol_qry <-
  function(id, from, to)
  {
    # convert "latestDate" into the Vegvesen-format
    stations_metadata_df <-
      stations_metadata_df %>% 
      mutate(
        latestData = format(stations_metadata_df$latestData,
                            format = "%Y-%m-%d %H:%M")
        ) 
    
    # return the following query
    # replace id, from, to into the string

    qry<-
      '
      {
        trafficData(trafficRegistrationPointId: glue("{id}")) {
          volume {
            byHour(from: glue("{from}"), to: glue("{to}")) {
              edges {
                node {
                  from
                  to
                  total {
                    volumeNumbers {
                      volume
                    }
                  }
                }
              }
            }
          }
        }
        
      }# qry end
      '
    
  }

# Error:
# Error: unexpected '{' in:
#"      {
#      trafficData(trafficRegistrationPointId: glue("{id}")) {"


   
# Verify
GQL(
  vol_qry(
    id=stations_metadata_df$id[1], 
    from=to_iso8601(stations_metadata_df$latestData[1],-4),
    to=to_iso8601(stations_metadata_df$latestData[1],0)
  ),
  .url = configs$vegvesen_url
)


# sorry I'm lost.



