module Infsabot.Board.Interface (
		Board(Board),
			boardSize, boardTime,
			(!!!), setRobot, robotAt, updateSpot, robotAlongPath,
			listOfRobots,
		startingBoard,
		GameSpot(GameSpot), toSeenSpot,
		assertRobotSourcesAgree, boardChecks
	) where

import Infsabot.Board.Logic
import Infsabot.Board.Tests
