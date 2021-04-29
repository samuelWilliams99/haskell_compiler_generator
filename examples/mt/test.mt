include "test2"
let
    var n := 0;
    var count := 10
in
    while count != 0 do begin
        n := n + 1;
        if (n/2) != ((n-1)/2) then begin
            print( n );
            count := count - 1
        end else pass
    end
