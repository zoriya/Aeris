import 'package:flutter/material.dart';

///Base class for every popup menu in application
class AerisPopupMenu extends StatelessWidget {
  ///Selection callback
  final void Function(Object?) onSelected;

  ///Icon of the popup menu
  final IconData icon;

  ///item builder
  final List<PopupMenuEntry<Object?>> Function(BuildContext) itemBuilder;

  ///Offset between popup and button
  final Offset menuOffset;
  const AerisPopupMenu(
      {Key? key,
      required this.onSelected,
      required this.icon,
      required this.itemBuilder,
      this.menuOffset = const Offset(0, 0)})
      : super(key: key);

  @override
  Widget build(BuildContext context) {
    return PopupMenuButton(
      itemBuilder: itemBuilder,
      onSelected: (route) => onSelected(route),
      offset: menuOffset,
      child: Icon(icon),
      shape: const RoundedRectangleBorder(
        borderRadius: BorderRadius.all(Radius.circular(20))),
    );
  }
}
