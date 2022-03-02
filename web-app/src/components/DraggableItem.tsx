import React, { useRef } from "react";

export interface DraggableItemProps {
	index: number;
	moveItem: any;
	children: React.ReactNode;
}

export const DraggableItemsList = ({ children, index, moveItem }: DraggableItemProps) => {
	return (<div/>);
};