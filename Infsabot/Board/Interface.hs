module Infsabot.Board.Interface (
		Board(Board),
			boardRobots, boardSize, boardTime,
			(!!!), setRobot, robotAt, updateSpot, robotAlongPath,
		startingBoard,
		GameSpot(GameSpot), toSeenSpot,
		assertRobotSourcesAgree
	) where

import Infsabot.Board.Logic
import Infsabot.Board.Tests
