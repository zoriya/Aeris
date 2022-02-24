import { GenericButtonProps } from "../components/GenericButton";
import { AppAREAType, ReactionTypeEnum, ActionTypeEnum, ParamTypeEnum, AppPipelineType, ImageProps, AppServiceType } from "./types";
import MoreVert from "@mui/icons-material/MoreVert";
import { API_ROUTE } from "..";



export const AppServicesLogos: { [key: string]: ImageProps } = {
	youtube: {
		imageSrc: "https://upload.wikimedia.org/wikipedia/commons/0/09/YouTube_full-color_icon_%282017%29.svg",
		altText: "YouTube logo",
	},
	spotify: {
		imageSrc: "https://upload.wikimedia.org/wikipedia/commons/8/84/Spotify_icon.svg",
		altText: "Spotify logo",
	},
	github: {
		imageSrc: "https://upload.wikimedia.org/wikipedia/commons/9/91/Octicons-mark-github.svg",
		altText: "GitHub logo",
	},
	twitter: {
		imageSrc: "https://upload.wikimedia.org/wikipedia/sco/9/9f/Twitter_bird_logo_2012.svg",
		altText: "Twitter logo",
	},
	discord: {
		imageSrc: "https://upload.wikimedia.org/wikipedia/fr/4/4f/Discord_Logo_sans_texte.svg",
		altText: "Discord logo",
	},
	gmail: {
		imageSrc: "https://upload.wikimedia.org/wikipedia/commons/7/7e/Gmail_icon_%282020%29.svg",
		altText: "GMail logo",
	},
};

const generateRandomString = (): string => {
	let randomString = '';
	const randomNumber = Math.floor(Math.random() * 10);

	for (let i = 0; i < 20 + randomNumber; i++) {
		randomString += String.fromCharCode(33 + Math.floor(Math.random() * 94));
	}
	return randomString;
}

export const getCookieValue = (name: string): string => {
	var nameEQ = name + "=";
	var ca = document.cookie.split(';');
	for(var i=0;i < ca.length;i++) {
		var c = ca[i].trim();
		if (c.indexOf(nameEQ) == 0)
			return c.substring(nameEQ.length,c.length);
	}
	return "";
};

export const sendServiceAuthToken = async (authToken: string, serviceEndpoint: string): Promise<boolean> => {
	const response = await fetch(API_ROUTE + serviceEndpoint + '?code=' + authToken, {
		method: 'GET',
		headers: {
			'Authorization': 'Bearer ' + getCookieValue('aeris_jwt')
		}
	});

	if (!response.ok) {
		console.log(response);
		return false;
	}
	return true;
};

export const AppServices: Array<AppServiceType> = [
	{
		label: "YouTube",
		uid: "youtube",
		logo: AppServicesLogos["youtube"],
		urlAuth: `https://accounts.google.com/o/oauth2/v2/auth?response_type=code&client_id=${process.env.GOOGLE_CLIENT_ID}&scope=openid%20email&redirect_uri=http://localhost:3000/authorization/google&state=${generateRandomString()}`,
		linked: false
	},
	{
		label: "Spotify",
		uid: "spotify",
		logo: AppServicesLogos["spotify"],
		urlAuth: `https://accounts.spotify.com/authorize?client_id=${process.env.SPOTIFY_CLIENT_ID}&response_type=code&redirect_uri=https://localhost:3000/authorization/spotify`,
		linked: false
	},
	{
		label: "GitHub",
		uid: "github",
		logo: AppServicesLogos["github"],
		urlAuth: `https://github.com/login/oauth/authorize?client_id=${process.env.GITHUB_CLIENT_ID}&response_type=code&redirect_uri=http://localhost:3000/authorization/github`,
		linked: false
	},
	{
		label: "Twitter",
		uid: "twitter",
		logo: AppServicesLogos["twitter"],
		urlAuth: `https://twitter.com/i/oauth2/authorize?response_type=code&client_id=${process.env.TWITTER_CLIENT_ID}&redirect_uri=https://localhost:3000/authorization/twitter&scope=tweet.read%20users.read%20offline.access&state=${generateRandomString()}&code_challenge=challenge&code_challenge_method=plain`,
		linked: true
	},
	{
		label: "Discord",
		uid: "discord",
		logo: AppServicesLogos["discord"],
		urlAuth: `https://discord.com/api/oauth2/authorize?response_type=code&client_id=${process.env.DISCORD_CLIENT_ID}&scope=applications.commands%20applications.entitlements%20applications.store.update%20bot%20guilds%20guilds.join%20guilds.members.read%20identify%20messages.read%20webhook.incoming&state=${generateRandomString()}`,
		linked: true
	},
	{
		label: "GMail",
		uid: "gmail",
		logo: AppServicesLogos["gmail"],
		urlAuth: "",
		linked: false
	},
];

export const ServiceActions: { [key: string]: Array<GenericButtonProps> } = {
	youtube: [
		{
			title: "Une vidéo à été publiée",
			service: AppServicesLogos["youtube"],
			trailingIcon: <MoreVert />,
		},
		{
			title: "Une vidéo à été likée",
			service: AppServicesLogos["youtube"],
			trailingIcon: <MoreVert />,
		},
		{
			title: "La playlist est mise à jour",
			service: AppServicesLogos["youtube"],
			trailingIcon: <MoreVert />,
		},
	],
	spotify: [
		{
			title: "Nouvelle musique d'un artiste",
			service: AppServicesLogos["spotify"],
			trailingIcon: <MoreVert />,
		},
		{
			title: "Playlist modifiée",
			service: AppServicesLogos["spotify"],
			trailingIcon: <MoreVert />,
		},
	],
	github: [
		{
			title: "Un repository a reçu une étoile",
			service: AppServicesLogos["github"],
			trailingIcon: <MoreVert />,
		},
		{
			title: "Une Pull Request à été ouverte",
			service: AppServicesLogos["github"],
			trailingIcon: <MoreVert />,
		},
		{
			title: "Push sur un repository",
			service: AppServicesLogos["github"],
			trailingIcon: <MoreVert />,
		},
	],
	twitter: [
		{
			title: "Vous êtes harcelé",
			service: AppServicesLogos["twitter"],
			trailingIcon: <MoreVert />,
		},
		{
			title: "Un compte vient de twitter",
			service: AppServicesLogos["twitter"],
			trailingIcon: <MoreVert />,
		},
	],
	discord: [
		{
			title: "Réception de demande d'ami",
			service: AppServicesLogos["discord"],
			trailingIcon: <MoreVert />,
		},
		{
			title: "Réception d'un message privé",
			service: AppServicesLogos["discord"],
			trailingIcon: <MoreVert />,
		},
		{
			title: "Vous avez été ping",
			service: AppServicesLogos["discord"],
			trailingIcon: <MoreVert />,
		},
	],
	gmail: [
		{
			title: "Réception d'un mail",
			service: AppServicesLogos["gmail"],
			trailingIcon: <MoreVert />,
		},
	],
};

export const AppListActions: Array<AppAREAType> = [
    {
		isAction: true,
        type: ActionTypeEnum.TwitterNewPost,
        params: {
            contents: {
                "author": {
                    value: "me",
                    description: "author of the post",
                    type: ParamTypeEnum.String
                }
            }
        },
        description: "Un nouveau tweet à été posté",
		service: AppServices[3]
    },
	{
		isAction: true,
        type: ActionTypeEnum.None,
        params: {
            contents: {
            }
        },
        description: "Ajouter une action",
		service: AppServices[0]
    }
]

export const AppListReactions: Array<AppAREAType> = [
    {
		isAction: false,
        type: ReactionTypeEnum.TwitterTweet,
        params: {
            contents: {
                "body": {
                    value: "yatta",
                    description: "The text to tweet",
                    type: ParamTypeEnum.String
                }
            }
        },
        description: "Création d'un tweet",
		service: AppServices[3]
    },
	{
		isAction: false,
        type: ReactionTypeEnum.None,
        params: {
            contents: {
            }
        },
        description: "Ajouter une réaction",
		service: AppServices[0]
    }
]

export const AppListPipelines: Array<AppPipelineType> = [
	{
		name: "my pipe",
		action: AppListActions[0],
		reactions: [
			AppListReactions[0]
		],
		data: {
			enabled: true,
			status: "il fait beau aujourd'hui",
			error: false
		}
	},
	{
		name: "nouvelle pipeline",
		action: AppListActions[1],
		reactions: [
			AppListReactions[1]
		],
		data: {
			enabled: true,
			status: "ninjago",
			error: false
		}
	}
]