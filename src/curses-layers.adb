------------------------------------------------------------------------------
--                                                                          --
--                        UNIX Terminal Control Package                     --
--                             (n)curses Binding                            --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2018, ANNEXI-STRAYLINE Trans-Human Ltd.                   --
--  All rights reserved.                                                    --
--                                                                          --
--  Original Contributors:                                                  --
--  * Richard Wai (ANNEXI-STRAYLINE)                                        --
--                                                                          --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--      * Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--                                                                          --
--      * Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--                                                                          --
--      * Neither the name of ANNEXI-STRAYLINE nor the names of its         --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     --
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       --
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A --
--  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL ANNEXI-STRAYLINE   --
--  BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR  --
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF    --
--  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR         --
--  BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,   --
--  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR --
--  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF  --
--  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                              --
--                                                                          --
------------------------------------------------------------------------------

package body Curses.Layers is
   
   --
   -- Rack_Iterator Implementation
   --
   
   ------------------
   -- Layer_Exists --
   ------------------
   function Layer_Exists (Position: Rack_Cursor) return Boolean is
   begin
      -- Always check the Rack_Cursor against the Rack's Sequence, unless the
      -- Link is null
      if         Position.Actual.Link /= null
        and then Position.Of_Rack.Actual.Sequence = Position.Actual.Sequence
      then
         return True;
         
      else
         return False;
      end if;
   end Layer_Exists;
   
      
   -----------
   -- First --
   -----------
   function First (Object: Rack_Iterator) return Rack_Cursor is
      ((Of_Rack => Object.The_Rack, Actual => Object.The_Rack.Actual.Head));
      
      
   ----------
   -- Next --
   ----------
   function Next (Object: Rack_Iterator; Position: Rack_Cursor)
                 return Rack_Cursor
   is
   begin
      if Object.The_Rack /= Position.Of_Rack then
         raise Constraint_Error with
           "Attempt to iterate Rack_Iterator object against a Rack_Cursor " &
           "which is not associated with the same Rack.";
      end if;
      
      return Cursor: Rack_Cursor (Object.The_Rack) do
        Cursor.Actual := Object.The_Rack.Actual.Next (Position.Actual);
        -- This checks the sequence, and raises a Constraint_Error if it is
        -- not current
      end return;
   end Next;
   
   
   ----------
   -- Last --
   ----------
   function Last (Object: Rack_Iterator) return Rack_Cursor is
      ((Of_Rack => Object.The_Rack, Actual => Object.The_Rack.Actual.Tail));
         
         
   --------------
   -- Previous --
   --------------
  function Previous (Object  : Rack_Iterator; 
                     Position: Rack_Cursor)
                    return Rack_Cursor
   is
   begin
      if Object.The_Rack /= Position.Of_Rack then
         raise Constraint_Error with
           "Attempt to iterate Rack_Iterator object against a Rack_Cursor " &
           "which is not associated with the same Rack.";
      end if;
      
      return Cursor: Rack_Cursor (Object.The_Rack) do
        Cursor.Actual := Object.The_Rack.Actual.Prev (Position.Actual);
        -- This checks the sequence, and raises a Constraint_Error if it is
        -- not current
      end return;
   end Previous;
   
   --
   -- Rack Public Interface
   --
   
   procedure Freeze (The_Rack: in out Rack) is
   begin
      The_Rack.Actual.Freeze;
   end Freeze;
   
   
   procedure Thaw (The_Rack: in out Rack) is
   begin
      The_Rack.Actual.Thaw;
   end Thaw;
   
   procedure Prepend (The_Rack   :         in out Rack;
                      The_Layer  : aliased in out Surface'Class; 
                      With_Freeze:         in     Boolean := False)
   is
   begin
      The_Rack.Actual.Prepend (The_Layer   => The_Layer,
                               With_Freeze => With_Freeze);
   end Prepend;
   
   procedure Append  (The_Rack   :         in out Rack;
                      The_Layer  : aliased in out Surface'Class;
                      With_Freeze:         in     Boolean := False)
   is
   begin
      The_Rack.Actual.Append (The_Layer   => The_Layer,
                              With_Freeze => With_Freeze);
   end Append;
   
   procedure Remove  (The_Rack   : in out Rack;
                      The_Layer  : in out Surface'Class;
                      With_Freeze: in     Boolean := False)
   is
   begin
      The_Rack.Actual.Remove (The_Layer   => The_Layer,
                              With_Freeze => With_Freeze);
   end Remove;
   
   
   procedure Promote   (The_Rack   : in out Rack;
                        The_Layer  : in out Surface'Class;
                        With_Freeze: in     Boolean := False)
   is
   begin
      The_Rack.Actual.Promote (The_Layer   => The_Layer,
                               With_Freeze => With_Freeze);
   end Promote;
   
   
   procedure Demote    (The_Rack   : in out Rack;
                        The_Layer  : in out Surface'Class;
                        With_Freeze: in     Boolean := False)
   is
   begin
      The_Rack.Actual.Demote (The_Layer   => The_Layer,
                              With_Freeze => With_Freeze);
   end Demote;
     
   
   procedure Above     (The_Rack       : in out Rack;
                        Subject, Object: in out Surface'Class;
                        With_Freeze    : in     Boolean := False)
   is
   begin
      The_Rack.Actual.Above (Subject     => Subject,
                             Object      => Object,
                             With_Freeze => With_Freeze);
   end Above;
   
   
   procedure Below     (The_Rack       : in out Rack;
                        Subject, Object: in out Surface'Class;
                        With_Freeze    : in     Boolean := False)
   is
   begin
      The_Rack.Actual.Below (Subject     => Subject,
                             Object      => Object,
                             With_Freeze => With_Freeze);
   end Below;
   
   
   procedure Superior  (The_Rack   : in out Rack;
                        The_Layer  : in out Surface'Class;
                        With_Freeze: in     Boolean := False)
   is
   begin
      The_Rack.Actual.Superior (The_Layer   => The_Layer,
                                With_Freeze => With_Freeze);
   end Superior;
   
   
   procedure Inferior  (The_Rack   : in out Rack;
                        The_Layer  : in out Surface'Class;
                        With_Freeze: in     Boolean := False)
   is
   begin
      The_Rack.Actual.Superior (The_Layer   => The_Layer,
                                With_Freeze => With_Freeze);
   end Inferior;
   
   
   procedure Hint_Changed (The_Rack: in out Rack; 
                           Changed :    out Boolean)
   is
   begin
      The_Rack.Actual.Hint_Changed (Changed);
   end Hint_Changed;
      
   --
   -- Rack Implementation
   --
   
   protected body Rack_Controller is
         
      --
      -- Consistency Tools
      --
      
      ------------
      -- Freeze --
      ------------
      entry Freeze when Lock < Natural'Last is
      begin
         Lock := Lock + 1;
      end Freeze;
      
      ----------
      -- Thaw --
      ----------
      procedure Thaw is
      begin
         if Lock > 0 then
            Lock := Lock - 1;
         end if;
      end Thaw;
      
      ----------------
      -- Lock_Count --
      ----------------
      function Lock_Count return Natural is (Lock);
      
      
      --
      -- Registration
      --
      
      -------------
      -- Prepend --
      -------------
      procedure Prepend_Actual (The_Layer: in out Surface'Class) is
         Element_Access: Surface_Access := The_Layer'Unchecked_Access;
      begin
         Change_Hint := True;
         
         if Rack_Head = null then  -- Empty rack
            Rack_Head := Element_Access;
            Rack_Tail := Element_Access;
            
         else
            -- This is safe by virtue of this protected type, as long as the 
            -- protected type is used to manage the list of objects,
            -- modification only occurs in protected procedures. Though we
            -- are not modifiying only data internal to the protected type,
            -- we can take advantage of the exclusivity that is provided by
            -- protected procedures generally
            Rack_Head.Prev := Element_Access;
            The_Layer.Next := Rack_Head;
            The_Layer.Prev := null;
            Rack_Head      := Element_Access;
            
         end if;
      end Prepend_Actual;
      
      ----------------------------------------
      entry Prepend (The_Layer  : in out Surface'Class;
                     With_Freeze: in     Boolean := False) 
      when Lock = 0 is
      begin
         Prepend_Actual (The_Layer);
         The_Sequence.Addition_Sequence := The_Sequence.Addition_Sequence + 1;
         
         if With_Freeze then
            -- We know for a fact that Lock is zero before this.
            -- No need to bother with addition here
            Lock := 1;
         end if;
      end Prepend;
      
      ------------
      -- Append --
      ------------
      procedure Append_Actual (The_Layer: in out Surface'Class) is
         Element_Access: Surface_Access := The_Layer'Unchecked_Access;
         
      begin
         Change_Hint := True;
         
         if Rack_Head = null then  -- Empty rack
            Rack_Head := Element_Access;
            Rack_Tail := Element_Access;
            
         else
            Rack_Tail.Next := Element_Access;
            The_Layer.Prev := Rack_Tail;
            The_Layer.Next := null;
            Rack_Tail      := Element_Access;
            
         end if;
      end Append_Actual;
      
      ----------------------------------------
      entry Append  (The_Layer  : in out Surface'Class;
                     With_Freeze: in     Boolean := False) 
      when Lock = 0 is
      begin
         Append_Actual (The_Layer);
         The_Sequence.Addition_Sequence := The_Sequence.Addition_Sequence + 1;
         
         if With_Freeze then
            -- We know for a fact that Lock is zero before this.
            -- No need to bother with addition here
            Lock := 1;
         end if;
      end Append;
      
      
      ------------
      -- Remove --
      ------------
      -- Internal use, legitimately used by the manipulation subprograms in
      -- automic operations. Requeing could destroy the atomicity required by
      -- such operations
      procedure Remove_Actual (The_Layer: in out Surface'Class) is
      begin
         Change_Hint := True;
         
         if The_Layer.Prev = null then
            if The_Layer.Next = null then
               -- This is one of two things:
               -- 1. The last (only) layer on the Rack or
               -- 2. A second removal of an already removed layer
               if Rack_Head = The_Layer'Unchecked_Access then
                  -- Manually clear the rack
                  Rack_Head := null;
                  Rack_Tail := null;
                  return;
               else
                  -- Already "removed". Protect the list from corruption.
                  return;
               end if;                  
               
            end if;
            
            Rack_Head      := The_Layer.Next;
            Rack_Head.Prev := null;
            
         elsif The_Layer.Next = null then
            Rack_Tail      := The_Layer.Prev;
            Rack_Tail.Next := null;
            
         else
            The_Layer.Next.Prev := The_Layer.Prev;
            The_Layer.Prev.Next := The_Layer.Next;
            
         end if;
         
         The_Layer.Next := null;
         The_Layer.Prev := null;
      end Remove_Actual;
      
      
      ----------------------------------------
      entry Remove (The_Layer  : in out Surface'Class;
                    With_Freeze: in     Boolean := False)
      when Lock = 0 is
      begin
         Remove_Actual (The_Layer);
         The_Sequence.Removal_Sequence := The_Sequence.Removal_Sequence + 1;
         
         if With_Freeze then
            -- We know for a fact that Lock is zero before this.
            -- No need to bother with addition here
            Lock := 1;
         end if;
      end Remove;
      
      
      --
      -- Manipulation
      --
      
      -------------
      -- Promote --
      -------------
      procedure Promote_Actual (The_Layer: in out Surface'Class) is
         Element_Access: Surface_Access := The_Layer'Unchecked_Access;
         
      begin
         if The_Layer.Prev = null then
            -- Nowhere to go
            return;
         end if;
         
         Change_Hint := True;
         
         -- our Previous needs to become our next, but our Previous' Previous
         -- needs to become our previous
         declare
            Old_Prev: Surface_Access := The_Layer.Prev;
         begin
            The_Layer.Prev := Old_Prev.Prev;
            
            if The_Layer.Prev /= null then
               The_Layer.Prev.Next := Element_Access;
            else
               Rack_Head := Element_Access;
            end if;
            
            Old_Prev.Prev := Element_Access;
            Old_Prev.Next := The_Layer.Next;
            
            if The_Layer.Next /= null then
               The_Layer.Next.Prev := Old_Prev;
            else
               Rack_Tail := Old_Prev;
            end if;
            
            The_Layer.Next := Old_Prev;
         end;
         
      end Promote_Actual;
      
      ----------------------------------------
      entry Promote (The_Layer  : in out Surface'Class;
                     With_Freeze: in     Boolean := False)
      when Lock = 0 is
      begin
         Promote_Actual (The_Layer);
         
         if With_Freeze then
            -- We know for a fact that Lock is zero before this.
            -- No need to bother with addition here
            Lock := 1;
         end if;
      end Promote;
      
      
      ------------
      -- Demote --
      ------------
      entry Demote (The_Layer  : in out Surface'Class;
                    With_Freeze: in     Boolean := False)
      when Lock = 0 is
      begin
         -- Demotion is just promoting our inferior (if we can)
         if The_Layer.Next /= null then
            Promote_Actual (The_Layer.Next.all);
         end if;
         
         if With_Freeze then
            -- We know for a fact that Lock is zero before this.
            -- No need to bother with addition here
            Lock := 1;
         end if;
      end Demote;
      
      
      -----------
      -- Above --
      -----------
      entry Above (Subject, Object: in out Surface'Class;
                   With_Freeze    : in     Boolean := False)
      when Lock = 0 is
         Subject_Access: Surface_Access := Subject'Unchecked_Access;
         Object_Access : Surface_Access := Object'Unchecked_Access;
      begin
         Remove_Actual (Subject);
         
         Subject.Next := Object_Access;
         Subject.Prev := Object.Prev;
         
         Object.Prev  := Subject_Access;
         
         if Subject.Prev = null then
            Rack_Head := Subject_Access;
         end if;
         
         if With_Freeze then
            -- We know for a fact that Lock is zero before this.
            -- No need to bother with addition here
            Lock := 1;
         end if;
      end Above;
      
      
      -----------
      -- Below --
      -----------
      entry Below (Subject, Object: in out Surface'Class;
                   With_Freeze    : in     Boolean := False)
      when Lock = 0 is
         Subject_Access: Surface_Access := Subject'Unchecked_Access;
         Object_Access : Surface_Access := Object'Unchecked_Access;
      begin
         Remove_Actual (Subject);
         
         Subject.Prev := Object_Access;
         Subject.Next := Object.Next;
         
         Object.Next  := Subject_Access;
         
         if Subject.Next = null then
            Rack_Tail := Subject_Access;
         end if;
         
         if With_Freeze then
            -- We know for a fact that Lock is zero before this.
            -- No need to bother with addition here
            Lock := 1;
         end if;
      end Below;
      
      
      --------------
      -- Superior --
      --------------
      entry Superior (The_Layer  : in out Surface'Class;
                      With_Freeze: in     Boolean := False)
      when Lock = 0 is
      begin
         -- If The_Layer is already Superior, don't waste any time
         if The_Layer.Prev /= null then
            -- Really a Remove followed by a Prepend
            Remove_Actual  (The_Layer);
            Prepend_Actual (The_Layer);
         end if;
         
         if With_Freeze then
            -- We know for a fact that Lock is zero before this.
            -- No need to bother with addition here
            Lock := 1;
         end if;
      end Superior;
      
      
      --------------
      -- Inferior --
      --------------
      entry Inferior (The_Layer  : in out Surface'Class;
                      With_Freeze: in     Boolean := False)
      when Lock = 0 is
      begin
         -- If The_Layer is already Inferior, don't waste any time
         if The_Layer.Next /= null then
            -- Really a Remove followed by an Append
            Remove_Actual (The_Layer);
            Append_Actual (The_Layer);
         end if;
         
         if With_Freeze then
            -- We know for a fact that Lock is zero before this.
            -- No need to bother with addition here
            Lock := 1;
         end if;
      end Inferior;
      
      
      --
      -- Traversal
      --
      
      ----------
      -- Head --
      ----------
      function Head return Link_Pack is ((Sequence => The_Sequence,
                                          Link     => Rack_Head));
      
      ----------
      -- Tail --
      ----------
      function Tail return Link_Pack is ((Sequence => The_Sequence,
                                          Link     => Rack_Tail));
      
      ----------
      -- Next --
      ----------
      function Next (From: Link_Pack) return Link_Pack is
      begin
         if From.Sequence /= The_Sequence then
            raise Constraint_Error with
              "Rack.Next invoked from an invalid sequence";
         end if;
         
         return Link_PacK'(Sequence => The_Sequence, 
                           Link     => From.Link.Next);
      end Next;
      
      
      ----------
      -- Prev --
      ----------
      function Prev (From: Link_Pack) return Link_Pack is
      begin
         if From.Sequence /= The_Sequence then
            raise Constraint_Error with
              "Rack.Prev invoked from an invalid sequence";
         end if;
         
         return Link_PacK'(Sequence => The_Sequence, 
                           Link     => From.Link.Prev);
      end Prev;
      
      -- Note that only Rack objects ever modifiy the Layer.Next/Prev links,
      -- making this safe as long as we ensure the sequence has not changed
      -- when we refrerence From. This assumption is why Layers is a private
      -- package. Two Layer objects sharing the same Rack would be a disaster
      -- so we simply bar the user from ever finding themselves in that
      -- situation
      
      --
      -- Hints
      --
      
      procedure Hint_Changed (Changed: out Boolean) is
      begin
         Changed     := Change_Hint;
         Change_Hint := False;
      end Hint_Changed;
      
      
      --
      -- Sequence
      --
      
      function Sequence return Sequence_Set is (The_Sequence);
      
   end Rack_Controller;
   
   
   ---------------
   -- Reference --
   ---------------
   
   -- Cursor reference
   function Reference (The_Rack: in out Rack; 
                       Position: in     Rack_Cursor)
                      return Layer_Reference
   is
   begin
      -- Check the validity of the Cursor before returning a reference. Note
      -- that this is really a partial protection, and all iteration on a Rack
      -- should always be done with the Rack frozen, but at least this helps
      -- protect in some cases. If the referenced object is removed just after
      -- returning, there is not much we can do. Our sequence numbers are really
      -- about ensuring Rack_Cursors are consistent between uses, and esepcially
      -- ensuring that any iteration over a changed rack is interrupted.
      
      if Position.Actual.Sequence /= The_Rack.Actual.Sequence then
         raise Constraint_Error with
           "Attempted reference of stale Rack_Cursor (sequence mismatch)";
         
      elsif not Layer_Exists (Position) then
         raise Constraint_Error with
           "Attempted reference of empty Rack_Cursor";
         
      end if;
      
      return Layer_Reference'(Reference => Position.Actual.Link);
      
   end Reference;
   
   
   -- Direct reference
   function Reference (The_Rack: in out Rack; 
                       Position: in     Direct_Layer_Select)
                      return Layer_Reference
   is
      Ref_Link: Link_Pack;
   begin
      if The_Rack.Empty then
         raise Constraint_Error with
           "Attempt to do an absolute reference on an empty Rack";
      end if;
      
      case Position is
         when Top_Layer =>
            Ref_Link := The_Rack.Actual.Head;
            
         when Bottom_Layer =>
            Ref_Link := The_Rack.Actual.Tail;
      end case;
      
      return Layer_Reference'(Reference => Ref_Link.Link);
   end Reference;
   
   
   -------------
   -- Iterate --
   -------------
   -- User-defined Iterator
   function Iterate (The_Rack: in out Rack) 
                    return Rack_Iterator_Interfaces.Reversible_Iterator'Class is
   begin
      return Rack_Iterator'(The_Rack => The_Rack'Access);
   end Iterate;
      
      
   -- General iteration
   procedure Iterate 
     (The_Rack: in out Rack;
      Action  : not null access procedure (The_Layer: in out Surface'Class))
   is
      Rack_Locked: Boolean := False;
   begin
      The_Rack.Freeze;
      Rack_Locked := True;
      
      for E of The_Rack loop
         Action (E);
      end loop;
      
      The_Rack.Thaw;
   exception
      when others =>
         if Rack_Locked then
            The_Rack.Thaw;
         end if;
   end Iterate;
   
end Curses.Layers;
