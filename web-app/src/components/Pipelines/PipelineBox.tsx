import Card from '@mui/material/Card';
import CardContent from '@mui/material/CardContent';
import CardMedia from '@mui/material/CardMedia';
import Typography from '@mui/material/Typography';
import { CardActionArea } from '@mui/material';

import { useTheme } from '@mui/material/styles';
import Box from '@mui/material/Box';
import ArrowForwardIcon from '@mui/icons-material/ArrowForward';

import "./PipelineBox.css"
import { borderRadius } from '@mui/system';
import type { ServiceProps } from './../types'


interface PipelineBoxProps {
	// title of the pipeline box
	title: string,
	// utc in seconds or anything useful to display
	statusText: string,
	service1: ServiceProps,
	service2: ServiceProps,
	onClickCallback?: React.MouseEventHandler<HTMLButtonElement>
}

export type { PipelineBoxProps }

export default function PipelineBox({ title, statusText, service1, service2, onClickCallback }: PipelineBoxProps) {
	const theme = useTheme();

	return (
		<Card sx={{ alignItems:"center", borderRadius:"15px" }}>
			<CardActionArea onClick={onClickCallback} >
				<Box sx={{ display: 'flex', flexDirection: 'row'}} >
					<Box sx={{ flexDirection: 'column', width:"70%" }}>
						<CardContent>
							<Typography variant="h5" noWrap align="left" >
								{ title }
							</Typography>
							<Typography variant="subtitle2" color="text.secondary" noWrap align="left"  >
								{ statusText }
							</Typography>
						</CardContent>
					</Box>
					<Box sx={{ display: 'flex', width:"30%" , flexDirection: 'row', padding:'10px', alignItems:"center", justifyContent:"center", flexWrap: "true" }}>
						<CardMedia
							component="img"
							sx={{ width: 50 }}
							image={ service1.imageSrc }
							alt={ service1.altText }
						/>
						<ArrowForwardIcon sx={{ height: 38, width: 38 }} />
						<CardMedia
							component="img"
							sx={{ width: 50 }}
							image={ service2.imageSrc }
							alt={ service2.altText }
						/>
					</Box>
				</Box>
			</CardActionArea>
		</Card>
	);
}