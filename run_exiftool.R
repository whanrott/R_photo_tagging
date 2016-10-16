run_exiftool <- function() {
  photo_loc <- "/media/willh/82788/SUB1"
  
  run_exif <- readline("Run Exiftool command? (n)")
  if(run_exif == "y") {
    ## run system command
    starttm <- Sys.time();
    print(c("started   running exiftool",format(starttm, "%a %b %d %X %Y")));
    # system2("sh", args = "get2_metadata.sh")
    
    system2("exiftool", args = c("-r -csv -ext jpg -common -xpkeywords -lastkeywordxmp -rating", photo_loc), stdout = "photos.txt");
    
    endtm <- Sys.time();
    print(c("completed running exiftool",format(endtm, "%a %b %d %X %Y")));
    print(c("process took", difftime(endtm, starttm), "Minutes"));
    rm("starttm", "endtm");
  }
  }
