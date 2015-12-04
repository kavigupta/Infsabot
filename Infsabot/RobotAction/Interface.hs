module Infsabot.RobotAction.Interface (
        RobotProgram, RobotProgramResult,
        KnownState(KnownState),
            peekAtSpot, material, stateLocation, stateAge, stateMemory, stateMessages,
        RobotAction(Die, Noop, MoveIn, Dig, Spawn, Fire, Send),
            SpawnAction(SpawnAction), SendAction(SendAction), FireAction(FireAction),
            newProgram, newAppearance, newMaterial, newMemory, newDirection,
            fireDirection, materialExpended,
            messageToSend, sendDirection,
        orderOfOperations,
        actionCost,
        robActChecks
    ) where

import Infsabot.RobotAction.Logic
import Infsabot.RobotAction.Tests
