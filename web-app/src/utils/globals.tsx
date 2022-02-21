import { ImageProps } from "../components/types";

export interface AppServiceType {
    label: string,
    uid: string,
    logo: ImageProps
}

export const AppServices:Array<AppServiceType> = [
    {
        label:"YouTube",
        uid: "youtube",
        logo: {
            imageSrc: "https://upload.wikimedia.org/wikipedia/commons/0/09/YouTube_full-color_icon_%282017%29.svg",
            altText: "YouTube logo"
        }
    },
    {
        label:"Spotify",
        uid: "spotify",
        logo: {
            imageSrc: "https://upload.wikimedia.org/wikipedia/commons/8/84/Spotify_icon.svg",
            altText: "Spotify logo"
        }
    },
    {
        label:"GitHub",
        uid: "github",
        logo: {
            imageSrc: "https://upload.wikimedia.org/wikipedia/commons/9/91/Octicons-mark-github.svg",
            altText: "GitHub logo"
        }
    },
    {
        label:"Twitter",
        uid: "twitter",
        logo: {
            imageSrc: "https://upload.wikimedia.org/wikipedia/sco/9/9f/Twitter_bird_logo_2012.svg",
            altText: "Twitter logo"
        }
    },
    {
        label:"Discord",
        uid: "discord",
        logo: {
            imageSrc: "https://upload.wikimedia.org/wikipedia/fr/4/4f/Discord_Logo_sans_texte.svg",
            altText: "Discord logo"
        }
    },
    {
        label:"GMail",
        uid: "gmail",
        logo: {
            imageSrc: "https://upload.wikimedia.org/wikipedia/commons/7/7e/Gmail_icon_%282020%29.svg",
            altText: "GMail logo"
        }
    }
]