
-- | Generate a PostGIS Tiger Database with the Dockerfile DSL
--
-- This example is non-trivial and still a work in progress.

module Main where

import Control.Monad
import Data.Docker


main :: IO ()
main = do
    let d = dockerfile test
    putStr d
    writeFile "Dockerfile" d

setLocaleUTF8 = do
  run "locale-gen --no-purge en_US.UTF-8"
  env "LC_ALL" "en_US.UTF-8"
  run "update-locale LANG=en_US.UTF-8"


runAptGetUpdate  = run "apt-get -y update"
runAptGetUpgrade = run "apt-get -y upgrade"


-- | Example
test :: Docker ()
test = do

  from "ubuntu:trusty"
  maintainer "Christopher Reichert <christopher@reichertbrothers.com>"

  -- run "echo" ["deb http://archive.ubuntu.com/ubuntu trusty main universe", ">", "/etc/apt/sources.list" ]
  run "echo \"deb http://archive.ubuntu.com/ubuntu trusty main universe\" > /etc/apt/sources.list"
  run "apt-get -y update"
  run "apt-get -y install wget"
  run "wget --quiet --no-check-certificate -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add -"
  run "echo \"deb http://apt.postgresql.org/pub/repos/apt/ trusty-pgdg main\" >> /etc/apt/sources.list"

  runAptGetUpdate
  runAptGetUpgrade
  setLocaleUTF8

  run "apt-get -y install postgresql-9.3 postgresql-contrib-9.3 postgresql-9.3-postgis-2.1 postgis"
  run "echo \"host    all             all             0.0.0.0/0               md5\" >> /etc/postgresql/9.3/main/pg_hba.conf"
  run "service postgresql start && /bin/su postgres -c \"createuser -d -s -r -l docker\" && /bin/su postgres -c \"psql postgres -c \\\"ALTER USER docker WITH ENCRYPTED PASSWORD 'docker'\\\"\" && service postgresql stop"
  run "echo \"listen_addresses = '*'\" >> /etc/postgresql/9.3/main/postgresql.conf"
  run "echo \"port = 5432\" >> /etc/postgresql/9.3/main/postgresql.conf"
  -- Install system dependencies.
  run "apt-get update"
  run "apt-get install -qq wget python unzip"

  -- Set the necessary Postgres environment.
  env "PGDATA" "/usr/local/pgsql/data"
  env "PGUSER" "docker"
  env "PGPASSWORD" "docker"

  run "echo 'host all docker ::1/0 md5' > /etc/postgresql/9.3/main/pg_hba.conf"
  run "echo 'local all docker md5' >> /etc/postgresql/9.3/main/pg_hba.conf"
  -- Download the TIGER PostGIS extension.
  run "wget -q http://postgis.net/stuff/postgis-2.1.5dev.tar.gz"

  run "tar xvfz postgis-2.1.5dev.tar.gz"

  -- Set the platform variables in the installation scripts.
  run "sed -i 's/pgsql-9.0\\///' postgis-2.1.5dev/extras/tiger_geocoder/tiger_2011/tiger_loader_2013.sql"
  run "sed -i '/PGUSER/d' postgis-2.1.5dev/extras/tiger_geocoder/tiger_2011/tiger_loader_2013.sql"
  run "sed -i '/PGPASSWORD/d' postgis-2.1.5dev/extras/tiger_geocoder/tiger_2011/tiger_loader_2013.sql"
  run "sed -i '/PGUSER/d' postgis-2.1.5dev/extras/tiger_geocoder/tiger_2011/create_geocode.sh"
  run "sed -i '/PGPASSWORD/d' postgis-2.1.5dev/extras/tiger_geocoder/tiger_2011/create_geocode.sh"
  run "sed -i '/\\${PSQL_CMD}/ s/-d/-v ON_ERROR_STOP=1 -d/' postgis-2.1.5dev/extras/tiger_geocoder/tiger_2011/create_geocode.sh"
  run "sed -i '/search_path/ s/#//' postgis-2.1.5dev/extras/tiger_geocoder/tiger_2011/create_geocode.sh"

  -- Create the geocoder PostGIS database and install the extension.
  run "service postgresql start && \\\n\
      \        createdb geocoder && \\\n\
      \        psql -d geocoder -f /usr/share/postgresql/9.3/contrib/postgis-2.1/postgis.sql && \\\n\
      \        psql -d geocoder -f /usr/share/postgresql/9.3/contrib/postgis-2.1/spatial_ref_sys.sql && \\\n\
      \        psql -d geocoder -c \"CREATE EXTENSION fuzzystrmatch\" && \\\n\
      \        cd postgis-2.1.5dev/extras/tiger_geocoder/tiger_2011 && ./create_geocode.sh && \\\n\
      \        service postgresql stop"

  -- Prepare the database for the TIGER data.
  run "mkdir -p /gisdata/temp"
  run "chmod 777 /gisdata"

  run "service postgresql start && \\\n\
      \        psql -d geocoder -o /gisdata/nationtemp.sh -c \"SELECT loader_generate_nation_script('sh');\" && \\\n\
      \        service postgresql stop\n\n"

  -- # generate nations scripts
  run "sed 's/\\s*+$//' /gisdata/nationtemp.sh | tail -n +3 | head -n -2 > /gisdata/generate_nation.sh"
  run "rm /gisdata/nationtemp.sh"
  run "chmod +x /gisdata/generate_nation.sh"
  run "chmod 777 /gisdata/generate_nation.sh"
  run "ls -al /gisdata/generate_nation.sh"
  run "cat /gisdata/generate_nation.sh\n\n"

  let createGenScript :: String -> String
      createGenScript st =
          "service postgresql start && psql -d geocoder -o /gisdata/temp.sh -c  \
          \ \"SELECT loader_generate_script(ARRAY['" ++ st ++ "'], 'sh');\" && \
          \ service postgresql stop"

  -- put any state here
  -- forM_ ["DC","IN","PA", "NY"] $ \state -> do
  forM_ ["DC"] $ \state -> do
      run $ createGenScript state
      run "sed 's/\\s*+$//' /gisdata/temp.sh | tail -n +3 | head -n -2 > /gisdata/generate.sh"
      run "rm /gisdata/temp.sh"
      run "chmod +x /gisdata/generate.sh"
      run "chmod 777 /gisdata/generate.sh"
      run "ls -al /gisdata/generate.sh"
      run "cat /gisdata/generate.sh"

      -- Generate the data and load it into the database
      -- TODO move nation_temp.sh
      run "service postgresql start && \
          \  psql -d geocoder -c \"ALTER DATABASE geocoder SET search_path=public, tiger;\" && \
          \  /gisdata/generate_nation.sh && \
          \  /gisdata/generate.sh && \
          \  psql -d geocoder -c \"SELECT install_missing_indexes();\" && \
          \  service postgresql stop\n\n"

  -- #RUN service postgresql start && \
  -- #        psql geocoder -c "SELECT pprint_addy(normalize_address('202 East Fremont Street, Las Vegas, Nevada 89101')) As pretty_address;" && \
  -- #        service postgresql stop

  -- # Test queries against new db
  run "service postgresql start && \
      \ psql -d geocoder -c \"SELECT g.rating, ST_X(g.geomout) As lon, ST_Y(g.geomout) As lat,(addy).address As stno, (addy).streetname As street,(addy).streettypeabbrev As styp, (addy).location As city, (addy).stateabbrev As st,(addy).zip FROM geocode(normalize_address('1731 New Hampshire Avenue Northwest, Washington, DC 20010'), 1) As g;\" && \\\n\
      \ service postgresql stop"

  expose 5432
  cmd ["service", "postgresql", "start"]
  return ()
