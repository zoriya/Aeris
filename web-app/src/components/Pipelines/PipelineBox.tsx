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

interface ServiceProps {
	// the image src preferable to use svg files
	imageSrc: string,
	// the alt text (screen readers, etc)
	altText: string
}

interface PipelineBoxProps {
	// title of the pipeline box
	title: string,
	// utc in seconds or anything useful to display
	statusText: string,
	service1: ServiceProps,
	service2: ServiceProps,
}

export type { PipelineBoxProps, ServiceProps}

export default function PipelineBox({ title, statusText, service1, service2 }: PipelineBoxProps) {
	const theme = useTheme();

	return (
		<Card sx={{ display: 'flex', alignItems:"center", borderRadius:"15px" }}>
			<CardActionArea>
				<Box sx={{ display: 'flex', flexDirection: 'column', flex: '2 0 auto', width:'60%' }}>
					<CardContent sx={{ flex: '1 0 auto' }}>
						<Typography variant="h3" noWrap align="left" >
							{ title }
						</Typography>
						<Typography variant="subtitle2" color="text.secondary" noWrap align="left"  >
							{ statusText }
						</Typography>
					</CardContent>
				</Box>
				<Box sx={{ display: 'flex', flex: '1 0 auto' , flexDirection: 'row', padding:'10px', alignItems:"center" }}>
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
			</CardActionArea>
		</Card>
	);
}