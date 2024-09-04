##Open targets database 
#source('biodb.r')


# Create a connection to our new database, CarsDB.db
# you can check that the .db file has been created on your working directory
opentargetdb_populate=function(sqlitefile){
  #library(RSQLite)
#  source('config.r')
#  source('bmes_download.r')
  opentarget.file=bmes_download('https://raw.githubusercontent.com/CBIIT/mtp-config/main/data/pmtl_v3.1.csv', paste0(config('biodbdir'), '/opentarget_v3.1.csv'))
  rs=read.delim(opentarget.file, sep=',')

  db=DBI::dbConnect(RSQLite::SQLite(),sqlitefile);
  DBI::dbWriteTable(db, "opentarget", rs)
  DBI::dbDisconnect(db);
}

opentargetdb_populateifneeded=function(){
  tryCatch( { dbfile=biodb_file('opentarget',downloadifmissing=T); }
, error = function(e) {
  print('---NOTICE: failed to get/download opentarget database. Attempting to create it...');
  dbfile=biodb_file('opentarget',downloadifmissing=F);
#  source('opentargetdb.r');
  opentargetdb_populate(dbfile);
} );
}
