pragma ada_2012;
with ada.streams;
with ada.finalization;
generic
   type key is private;
   type value is private;
   no_value : value;
   level : positive;
   with function "=" (l,r : key) return boolean is <>;
   with function ">" (l,r : key) return boolean is <>;
   with function "<" (l,r : key) return boolean is <>;
   with function ">=" (l,r : key) return boolean is <>;
package skiplist is
   pragma elaborate_body;
   type list_t is new ada.finalization.limited_controlled with private;
   procedure insert(l : in out list_t; k : in key; v : in value; r : out boolean);
   procedure remove(l : in out list_t; k : in key; r : out  boolean);
   function search(l : list_t; k : key) return value;
   function size(l : list_t) return integer;
   procedure write(stream : not null access ada.streams.root_stream_type'class; item : list_t);
   for list_t'write use write;

private
   type node_t;
   type node_a is access node_t;
   type nodes_t is array (positive range <>) of node_a;
   type nodes_a is access nodes_t;
   type node_t is record
      k : key;
      v : value;
      forward : nodes_a;
   end record;
   type list_t is new ada.finalization.limited_controlled with record
      header	 : node_a;
      size	 : integer;
      list_level : integer;
   end record;
   overriding procedure initialize ( l : in out list_t);
   overriding procedure finalize (l : in out list_t);
end skiplist;
