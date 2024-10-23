# These commands extend the base image so that devtools::check(remote = TRUE, manual = TRUE) works
apt-get install libxml2-dev libfontconfig1-dev libssl-dev libcurl4-openssl-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev tidy
apt install texlive-latex-base texlive-fonts-recommended texlive-fonts-extra
# mktexlsr (maybe?)
tlmgr init-usertree
tlmgr install xkeyval etoolbox
