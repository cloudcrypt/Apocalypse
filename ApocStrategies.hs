
module ApocStrategies(
	strategies
	) where
	
strategies  :: [(String,Chooser)]
strategies = [("human",whiteHuman)]