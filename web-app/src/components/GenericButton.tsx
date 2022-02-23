import { Box, CardContent, Typography, CardMedia, IconButton, CardActionArea } from "@mui/material";
import Card from "@mui/material/Card";
import type { ImageProps } from "./types";

interface GenericButtonProps {
	title: string,
	service: ImageProps,
	trailingIcon: JSX.Element,
	onClickCallback?: React.MouseEventHandler<HTMLButtonElement>,
}

export type { GenericButtonProps };

export default function GenericButton({ title, service, trailingIcon, onClickCallback }: GenericButtonProps) {
	return (
		<Card
			sx={{
				display: "flex",
				alignItems: "center",
				borderRadius: "15px",
				width: "500px",
			}}>
			<CardActionArea onClick={onClickCallback} >
				<Box sx={{ display: "flex", flexDirection:"row" }}>
					<Box
						className="GenericButtonMedia"
						sx={{
							display: "flex",
							width: "15%",
							alignItems: "center",
							justifyContent: "center",
						}}>
						<CardMedia component="img" sx={{ width: "50%" }} image={service.imageSrc} alt={service.altText} />
					</Box>
					<Box sx={{ width: "70%", alignItems: "center", justifyContent: "center" }}>
						<CardContent>
							<Typography variant="h6" noWrap align="center">
								{title}
							</Typography>
						</CardContent>
					</Box>
					<Box
						sx={{
							width: "15%",
							display: "flex",
							alignItems: "center",
							justifyContent: "center",
						}}>
						<IconButton color="secondary" aria-label="Options" component="span">
							{trailingIcon}
						</IconButton>
					</Box>
				</Box>
			</CardActionArea>
		</Card>
	);
}
