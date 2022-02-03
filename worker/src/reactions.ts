import { PipelineEnv } from "./models/pipeline";

export type ReactionHandler = (params: any) => Promise<PipelineEnv>;
export type ReactionHandlers = {[key: string]: ReactionHandler}

export const reactionHandlers: ReactionHandlers = {

}
