if [ ! -d linux ] ; then
	git clone https://github.com/torvalds/linux.git
fi

docker build -t ubuntu-linux .