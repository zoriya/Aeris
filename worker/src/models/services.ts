import { Twitter } from "../services/twitter";
import { Youtube } from "../services/youtube";
import { ServiceCreator } from "./base-service";
import { ServiceType } from "./pipeline";

export const servicesFactory: ServiceCreator = {
	[ServiceType.Twitter]: (pipeline) => new Twitter(pipeline),
	[ServiceType.Youtube]: (pipeline) => new Youtube(pipeline),
};

