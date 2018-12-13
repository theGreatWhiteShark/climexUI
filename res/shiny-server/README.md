This is a short guide how to install and run the climex server on your
computer. Since we will use a shiny-server, it is not just accessible
via localhost but throughout your entire network.

## Prerequisites

For this tutorial it is expected you already followed all the steps in
the [installation
guide](https://github.com/rstudio/shiny-server/wiki/Building-Shiny-Server-from-Source)
of the
[shiny-server](https://www.rstudio.com/products/shiny/shiny-server/).

In addition, you have to have both the
[climex](https://gitlab.com/theGreatWhiteShark/climex) and the
[climexUI](https://gitlab.com/theGreatWhiteShark/climexUI) package
install on your system.

```{bash}
## Switching to the superuser
sudo su

## Starting R (if you have your own compiled version, make sure its 
## linked against /usr/local/bin/R).
R --no-init-file
```

```{R}
## Inside R get the newest version of the devtools package
install.packages( "devtools" )

## Installing the newest climex version on the master branch
devtools::install_gitlab( "theGreatWhiteShark/climex" )
devtools::install_gitlab( "theGreatWhiteShark/climexUI" )
```

## Configuration

In order to run the server, you have to copy all its configuration
files and scripts into shiny-server's directories

```{bash}
## Climex server configuration files and resources
sudo cp --recursive climex/ assets/ /srv/shiny-server/

## Configuration of the local R environment of the shiny user needed
## start up the shiny-server
sudo cp .Rprofile /home/shiny/.Rprofile

## Change the owner of all those files to the shiny user
sudo chown shiny /home/shiny/.Rprofile /srv/shiny-server/*
```

Finally, you need to store your data in the proper format (see
[vignette](../../vignettes/climex_app.Rmd) of the package) into a file
called _input.RData_ in the _assets_ folder.
