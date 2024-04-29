test:
	python3 ./build/build.py $(TASK_NAME)

clean:
	rm test.scm output/*

a4:
	$(MAKE) test TASK_NAME=a4