all: get-data/.done

get-data/.done:
	$(MAKE) -C get-data
