
cd /d/Documents/GitHub/ssm-nat-dp/
git init
git pull
git checkout master
git add "macau-all-people-nat.html"
git status
git commit -m "html refresh"
git push https://github.com/adamzerg/ssm-nat-dp.git
git merge gh-pages
git push https://github.com/adamzerg/ssm-nat-dp.git master:gh-pages