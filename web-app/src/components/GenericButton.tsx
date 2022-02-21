import { Box, CardContent, Typography, CardMedia, IconButton } from "@mui/material";
import Card from "@mui/material/Card";
import type { ServiceProps } from "./types";

interface GenericButtonProps {
	title: string;

	service: ServiceProps;

	trailingIcon: JSX.Element;
}

export type { GenericButtonProps };

export default function GenericButton({ title, service, trailingIcon }: GenericButtonProps) {
	return (
		<Card
			sx={{
				display: "flex",
				alignItems: "center",
				borderRadius: "15px",
				width: "500px",
			}}>
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
		</Card>
	);
}
