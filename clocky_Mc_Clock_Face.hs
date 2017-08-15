whatTimeIsIt :: Float -> String
whatTimeIsIt angle = show hour ++ ":" ++ show (60 - min)
  where everyHourAngle = 360 / 12
        everyMinAngle = 360 / 60
        hour = angle / everyHourAngle
        min = (angle - hour * everyHourAngle) / everyMinAngle