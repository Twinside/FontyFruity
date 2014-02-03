module Graphics.Text.TrueType.Bytecode where

type InstructionFlag = Bool

data Instruction =
    -- | Set Vectors To Coordinate axis.
    -- Apply to freedom & projection vector
      SVTCA InstructionFlag
    -- | Set projection vector to coordinate axis.
    -- Apply to projection vector
    | SPVTCA InstructionFlag
    -- | Set Freedom Vector To Coordinate Axis.
    -- Apply to the freedom vector
    | SFVTCA InstructionFlag
    -- | Set Projection Vector To Line.
    -- Apply to projection vector
    | SPVTL InstructionFlag
    -- | Set Freedom Vector To Line.
    -- Apply to freedom vector
    | SFVTL InstructionFlag
    -- | Set Freedom Vector To Projection Vector.
    -- Apply to freedom vector
    | SFVTPV
 	-- | Set Dual Projection Vector To Line
 	-- dual projection vector
 	| SDPVTL
 	-- | Set Projection Vector To Line.
 	-- Apply to projection vector
 	| SVPTL
 	-- | Set Projection Vector From Stack.
 	-- Apply to projection vector
 	| SPVFS
 	-- | Set Freedom Vector From Stack.
 	-- Apply to freedom vector
 	| SFVFS
 	-- | Set reference point 0.
 	-- Apply to rp0
 	| SRP0
 	-- | Set reference point 1.
 	-- Apply to rp1
 	| SRP1
 	-- | Set reference point 2.
 	-- Apply to rp2
 	| SRP2
 	-- | Set zone pointer 0.
 	-- Apply to zp0.
 	| SZP0
 	-- | Set zone pointer 1.
 	-- Apply to zp1.
 	| SZP1
 	-- | Set zone pointer 2.
 	-- Apply to zp2.
 	| SZP2
 	-- | Set zone pointers
 	-- Apply to zp0, zp1, zp2
 	| SZPS
    -- | Round To Half Grid.
    -- Apply to round state.
    | RTHG
    -- | Round To Grid.
    -- Apply to round state.
    | RTG
    -- | Round To Double Grid.
    -- Apply to round state.
    | RTDG
    -- | Round up To Grid.
    -- Apply to round state.
    | RUTG
    -- | Round down to grid.
    -- Apply to round state.
    | RDTG
    -- | Set rounding off
    -- Apply to rounds tate.
    | ROFF
    -- | Super round
    -- Apply to rounds tate.
    | SROUND
    -- | Super 45 round
    -- Apply to rounds tate.
    | S45ROUND
    -- | Set loop.
    -- Apply to loop
    | SLOOP
    -- | Set Single Width Cut-In
    -- Apply to single width cut-in
    | SSWCI
    -- | Set Control Value Table Cut-In
    -- Apply to control value cut-in
    | SCVTCI
    -- | Set Minimum Distance
    -- Apply to minimum distance
    | SMD
    -- | Get Freedom vector
    | GFV
    -- | Get Projection vector
    | GPV
    -- | Get Information
    | GETINFO
    -- | Measure Pixels Per EM
    | MPPEM
    -- | Measure Point size
    | MPS

    -- | If test
    | IF
    -- | Else
    | ELSE
    -- | Jump relative on false
    | JROF
    -- | Jump relative on true
    | JROT
    -- | Jump relative
    | JMPR
    -- | Loop and call
    | LOOPCALL

