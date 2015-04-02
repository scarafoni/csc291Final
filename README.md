# csc291Final
the final project for csc291 lisp
 (edit this one guys) I am editing the README


Goals:
One "human agent"- main goal is to survive, secondary is to escape. Rely on the "mood meter" to determine his course of action. Could have multiple paths of escape, but chooses between them depending on his surroundings. Human should also have option to wait if two things have equal importance (If you see food that is near a zombie, wait to see if it leaves). To avoid possible complications, weight decisions that could conflict, like which escape method to take. IF HAVE TIME: Can also have parameters such as food and water that can possibly grow strong enough to override the escape goal.

Perception- both humans and zombies can see around them, but human has greater range. The zombie uses perception only to detect humans, while the human can also use it to find exits, weapons, etc.

Several zombies- should have two modes: Wander randomly or chase human (same movement speeds, but lower perception)

Random events: Fear meter determines whether the human wants to open a crate, agent can make decisions on whether to open it or not.

Fear meter- basis for all decisions. Gets more aggressive/passive based on it

Environment- indoors, something that allows for narrow corridors and small outdoor spaces. Not too big, not too small. Every room/corridor is split into smaller squares to allow for proximity to zombies without an automatic encounter




Tasks:
1. Design and implement basic world in order to have a template
2. Implement the multiple agent world. Note: We do not know whether or not we will actually have multiple agents
3. Implement basic (non-agent) zombie, including wander/chase

