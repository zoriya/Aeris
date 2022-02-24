import { GenericButtonProps } from "../components/GenericButton";
import { AppAREAType, ReactionTypeEnum, ActionTypeEnum, ParamTypeEnum, AppPipelineType, ImageProps, AppServiceType } from "./types";
import MoreVert from "@mui/icons-material/MoreVert";



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

export const AppServices: Array<AppServiceType> = [
	{
		label: "YouTube",
		uid: "youtube",
		logo: AppServicesLogos["youtube"],
		urlAuth: "",
		linked: false
	},
	{
		label: "Spotify",
		uid: "spotify",
		logo: AppServicesLogos["spotify"],
		urlAuth: "",
		linked: false
	},
	{
		label: "GitHub",
		uid: "github",
		logo: AppServicesLogos["github"],
		urlAuth: "https://github.com/login/oauth/authorize?client_id=Iv1.c32d1b598854bac9&response_type=code&redirect_uri=http://localhost:3000/authorization/github",
		linked: false
	},
	{
		label: "Twitter",
		uid: "twitter",
		logo: AppServicesLogos["twitter"],
		urlAuth: "",
		linked: true
	},
	{
		label: "Discord",
		uid: "discord",
		logo: AppServicesLogos["discord"],
		urlAuth: "",
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