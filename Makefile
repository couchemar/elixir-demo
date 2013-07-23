node1:
	@iex --sname node1 --erl "-config conf1" -S mix

node2:
	@iex --sname node2 --erl "-config conf2" -S mix

node3:
	@iex --sname node3 --erl "-config conf3" -S mix

release:
	@mix relex.assemble

rel1:
	./rel/kvs/bin/iex --sname node1 --erl "-config conf1"

rel2:
	./rel/kvs/bin/iex --sname node2 --erl "-config conf2"

rel3:
	./rel/kvs/bin/iex --sname node3 --erl "-config conf3"
