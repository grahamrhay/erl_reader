echo "deb http://packages.erlang-solutions.com/debian wheezy contrib" | sudo tee /etc/apt/sources.list.d/erlang.list
wget -q -O- "http://packages.erlang-solutions.com/debian/erlang_solutions.asc" | sudo apt-key add -
sudo apt-get update
sudo apt-get install erlang
