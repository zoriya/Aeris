import 'package:aeris/src/widgets/aeris_card_page.dart';
import 'package:flutter/material.dart';
import 'package:aeris/src/views/setup_action_page.dart';
import 'package:aeris/src/widgets/aeris_popup_menu.dart';
import 'package:aeris/src/widgets/aeris_popup_menu_item.dart';
import 'package:aeris/src/models/action.dart' as aeris;
import 'package:flutter_gen/gen_l10n/app_localizations.dart';

/// [StatelessWidget] displayed as a PopupMenu
class ActionCardPopupMenu extends StatelessWidget {
  ActionCardPopupMenu({
    Key? key,
    required this.action,
    required this.then,
    required this.deletable,
    this.onDelete,
  }) : super(key: key) {
    if (deletable) {
      assert(onDelete != null);
    }
  }

  /// Action to trigger
  final aeris.Action action;

  /// Function to trigger when PopupMenu option is selected
  final void Function() then;

  /// Deletable characteristic
  final bool deletable;

  final void Function()? onDelete;

  @override
  Widget build(BuildContext context) {
    return AerisPopupMenu(
        onSelected: (value) {
          dynamic Function() callback = value! as dynamic Function();
          callback();
        },
        icon: Icons.more_vert,
        itemBuilder: (context) => [
              AerisPopupMenuItem(
                  context: context,
                  icon: Icons.settings,
                  title: AppLocalizations.of(context).modify,
                  value: () => showAerisCardPage(
                      context, (_) => SetupActionPage(action: action)).then((_) => then())
              ),
              AerisPopupMenuItem(
                context: context,
                icon: Icons.delete,
                title: AppLocalizations.of(context).delete,
                value: onDelete,
                enabled: deletable

                /// TODO delete from db
                // TODO Delete from parent pipeline
              ),
            ]);
  }
}
