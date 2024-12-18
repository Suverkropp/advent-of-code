#!/usr/bin/env bash

year="$1"
day="$2"

file="src/Y$year/Day$day.hs"

touch $file
cat > $file << EOL
module Y$year.Day$day (day$day) where

import AoC

day$day :: AoC String Int Int
day$day =
  AoC
    { year = $year,
      day = $day,
      handleInput = id,
      part1 = _,
      part2 = _
    }
EOL
