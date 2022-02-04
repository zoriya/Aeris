import { Twitter } from "./services/twitter";
import { PipelineEnv, ReactionType } from "./models/pipeline";

export type ReactionHandler = (params: any) => Promise<PipelineEnv>;
export type ReactionHandlers = {[key: string]: ReactionHandler}

export const reactionHandlers: ReactionHandlers = {
	[ReactionType.Twitter_Tweet]: Twitter.reactTweet
}
