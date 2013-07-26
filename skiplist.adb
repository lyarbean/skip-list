with ada.numerics.discrete_random;
with ada.unchecked_deallocation;
with ada.streams;
package body skiplist is
   subtype level_t is integer range 1..level;
   package randomizer is new ada.numerics.discrete_random (level_t);
   g : randomizer.generator;
   update : array (1 .. level) of node_a := (others => null);
   ---------------------------------------------------------------------------------
   function random_level return integer is
      l : integer := 1 ;
   begin
      while randomizer.random(g)  < level / 2 loop
	 exit when l >= level;
	 l := l + 1;
      end loop;
      return l;
   end random_level;
   ---------------------------------------------------------------------------------
   procedure free is new ada.unchecked_deallocation(object => node_t, name => node_a);
   procedure free is new ada.unchecked_deallocation(object => nodes_t, name => nodes_a);
   ---------------------------------------------------------------------------------
   procedure initialize (l : in out list_t) is
   begin
      randomizer.reset(g);
      l.header := new node_t;
      l.header.v := no_value;
      l.header.forward := new nodes_t(1 .. level);
      l.header.forward.all := (others => null);
      l.list_level := 1;
      l.size := 0;
   end initialize;

   procedure finalize (l : in out list_t) is
      x, y : node_a;
   begin
      x := l.header.forward(1);
      while x /= null loop
	 y := x.forward(1);
	 if x.forward /= null then
	    free(x.forward);
	 end if;
	 free(x);
	 x := y;
      end loop;
      free(l.header.forward);
      free(l.header);
   end finalize;
   ---------------------------------------------------------------------------------
   procedure insert(l : in out list_t; k : in key; v : in value; r : out boolean) is
      x : node_a;
      new_level : integer;
   begin
      r := false;
      x := l.header;
      if x = null then
	 raise program_error;
      end if;

      for j in reverse 1 .. l.list_level loop
	 loop
	    exit when x.forward(j) = null;
	    exit when x.forward(j).k >= k;
	    x := x.forward(j);
	 end loop;
	 update(j) := x;
      end loop;

      x := x.forward(1);
      if x /= null and then x.k = k then r := true; return; end if; -- fixme update x.v := v

      new_level := random_level;
      if new_level > l.list_level then
	 update(l.list_level + 1 .. new_level) := (others => l.header);
	 l.list_level := new_level;
      end if;
    
      x := new node_t;
      x.k := k;
      x.v := v;
      x.forward := new nodes_t(1 .. new_level);
      for j in 1..new_level loop
	 x.forward(j) := update(j).forward(j);
	 update(j).forward(j) := x;
      end loop;
      r :=  true;
      l.size := l.size + 1;
   end insert;

   ---------------------------------------------------------------------------------
   procedure remove(l : in out list_t; k : in key ; r : out boolean) is
      x : node_a;
   begin
      x := l.header;
      for j in reverse 1 .. l.list_level loop
	 loop
	    exit when x.forward(j) = null;
	    exit when x.forward(j).k >= k;
	    x := x.forward(j);
	 end loop;
	 update(j) := x;
      end loop;
      x := x.forward(1);
      r := false;
      if x.k = k then
	 for j in 1 .. l.list_level loop
	    exit when update(j).forward(j) /= x;
	    update(j).forward(j) := x.forward(j);
	 end loop;
	 free(x.forward);
	 free(x);
	 while l.list_level > 0 and then l.header.forward(l.list_level) = l.header loop
	    l.list_level := l.list_level - 1;
	 end loop;
	 r := true;
	 l.size := l.size - 1;
      end if;
   end remove;
   ---------------------------------------------------------------------------------
   function search(l : list_t; k : key) return value is
      x : node_a;
   begin
      x := l.header;
      for j in reverse 1 .. l.list_level loop
	 loop
	    exit when x.forward(j) = null;
	    exit when x.forward(j).k >= k;
	    x := x.forward(j);
	 end loop;
      end loop;
      x := x.forward(1);
      if x.k = k then return x.v; end if;
      return no_value;
   end search;
   ---------------------------------------------------------------------------------
   function size ( l : list_t) return integer is 
   begin
      return l.size;
   end size;
   ---------------------------------------------------------------------------------
   procedure write(stream : not null access ada.streams.root_stream_type'class; item : list_t) is
      x : node_a;
   begin
      for j in reverse 1 .. item.list_level loop
	 x := item.header.forward(j);
	 string'write(stream, ascii.lf & "-- level " & j'img & ascii.lf);
	 loop
	    exit when x = null;
	       string'write(stream, "[");
	       key'write(stream, x.k);
	       string'write(stream, " :");
	       value'write(stream, x.v);
	       string'write(stream, " ] ");
	    x := x.forward(j);
	 end loop;
      end loop;
   end write;
   ---------------------------------------------------------------------------------
   ---------------------------------------------------------------------------------
end skiplist;
