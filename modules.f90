   module text_transfer
      integer(4),parameter::ncresults=250
      contains
      function ru_doswin(string,dos_win)
      character(ncresults)::ru_doswin
      character(*),intent(in)::string
      logical(4),intent(in)::dos_win
      integer(2)::i,dos_win_code,dif
      ru_doswin=string
      do i=1,len_trim(ru_doswin)
          dos_win_code=iachar(ru_doswin(i:i))
          dif=0
          if(dos_win) then
              select case(dos_win_code)
              case(128:175)
                  dif=64
              case(224:239)
                  dif=16
              end select
          else
          select case(dos_win_code)
          case(192:239)
              dif=-64
          case(240:255)
              dif=-16
          end select
          end if
          if(dif/=0) ru_doswin(i:i)=char(dos_win_code+dif)
          end do
      end function ru_doswin
      end module text_transfer