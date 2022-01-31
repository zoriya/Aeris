import 'package:flutter/cupertino.dart';
import 'package:mobile/src/models/action.dart' as aeris;
import 'package:mobile/src/widgets/aeris_card_page.dart';

/// Class to get the action in route's arguments
class SetupActionPageArguments {
  final aeris.Action action;

  SetupActionPageArguments(this.action);
}

// Page to setup an action
class SetupActionPage extends StatelessWidget {
  const SetupActionPage({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    final SetupActionPageArguments arguments =
        ModalRoute.of(context)!.settings.arguments as SetupActionPageArguments;

    aeris.Action action = arguments.action;

    return AerisCardPage(body: Container());

  }
}
