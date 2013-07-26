with skiplist;
with ada.streams.stream_io;
with ada.text_io.text_streams;
use ada.text_io;
procedure skiplistdriver is
   std_out : access ada.streams.root_stream_type := ada.text_io.text_streams.stream(standard_output);
   type key_t is new integer;
   procedure write_int(stream : not null access ada.streams.root_stream_type'class; item : key_t);
   for key_t'write use write_int;
   procedure write_int(stream : not null access ada.streams.root_stream_type'class; item : key_t) is
   begin
      string'write(stream, item'img);
   end write_int;

   package ic_skiplist is new skiplist(
      key => key_t,
      value => key_t,
      no_value => -1,
      level => 9);
   use ic_skiplist;
   the_skiplist : list_t;
   r : boolean;
begin
   for j in key_t range 1..2**10 loop
      the_skiplist.insert(j, (j-2**9)**2,r);
   end loop;
   list_t'write(std_out,the_skiplist);
   new_line;
   for j in key_t range 2**6..2**9 loop
      the_skiplist.remove(j,r);
   end loop;
   list_t'write(std_out,the_skiplist);
   new_line;
end skiplistdriver;
