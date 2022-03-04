import 'package:aeris/src/aeris_api.dart';
import 'package:aeris/src/providers/action_catalogue_provider.dart';
import 'package:flutter/material.dart';
import 'package:flutter_form_builder/flutter_form_builder.dart';
import 'package:form_builder_validators/form_builder_validators.dart';
import 'package:get_it/get_it.dart';
import 'package:provider/provider.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:flutter_gen/gen_l10n/app_localizations.dart';

/// Floating Action button to access the setup API route modal
class SetupAPIRouteButton extends StatefulWidget {
  ///Can the app access the api with the current baseRoute?
  bool connected;
  void Function() onSetup;
  SetupAPIRouteButton(
      {Key? key, required this.connected, required this.onSetup})
      : super(key: key);

  @override
  State<SetupAPIRouteButton> createState() => _SetupAPIRouteButtonState();
}

class _SetupAPIRouteButtonState extends State<SetupAPIRouteButton> {
  @override
  Widget build(BuildContext context) {
    return FloatingActionButton(
      onPressed: () => showDialog(
              context: context, builder: (_) => const SetupAPIRouteModal())
          .then((_) => widget.onSetup()),
      backgroundColor: Theme.of(context).colorScheme.secondary,
      elevation: 10,
      child: Icon(widget.connected == true
          ? Icons.wifi
          : Icons.signal_cellular_connected_no_internet_0_bar_sharp),
    );
  }
}

/// Modal to setup route to connect to api
class SetupAPIRouteModal extends StatefulWidget {
  const SetupAPIRouteModal({Key? key}) : super(key: key);

  @override
  State<SetupAPIRouteModal> createState() => _SetupAPIRouteModalState();
}

class _SetupAPIRouteModalState extends State<SetupAPIRouteModal> {
  bool? connected;
  final _formKey = GlobalKey<FormBuilderState>();

  @override
  void initState() {
    super.initState();
    GetIt.I<AerisAPI>().getAbout().then((value) {
      setState(() {
        connected = value.isNotEmpty;
      });
    });
  }

  @override
  Widget build(BuildContext context) {
    return AlertDialog(
        title: Text(AppLocalizations.of(context).setupAPIRoute),
        content: FormBuilder(
          key: _formKey,
          child: FormBuilderTextField(
              initialValue: GetIt.I<AerisAPI>().baseRoute,
              name: "route",
              validator: FormBuilderValidators.required(context),
              decoration: InputDecoration(
                  labelText: AppLocalizations.of(context).routeToApi,
                  helperText: "Ex: http://host:port")),
        ),
        actionsAlignment: MainAxisAlignment.spaceEvenly,
        actions: [
          ElevatedButton(
            child: Text(AppLocalizations.of(context).tryToConnect),
            onPressed: () {
              _formKey.currentState!.save();
              if (_formKey.currentState!.validate()) {
                var route = _formKey.currentState!.value['route'];
                if (Uri.tryParse(route) == null) {
                  setState(() => connected = false);
                } else {
                  final oldRoute = GetIt.I<AerisAPI>().baseRoute;
                  GetIt.I<AerisAPI>().baseRoute = route;
                  setState(() {
                    connected = null;
                  });
                  GetIt.I<AerisAPI>().getAbout().then((value) {
                    setState(() {
                      connected = value.isNotEmpty;
                    });
                  }, onError: (_) => GetIt.I<AerisAPI>().baseRoute = oldRoute);
                }
              }
            },
          ),
          ElevatedButton(
            child: Text(connected == null
                ? AppLocalizations.of(context).loading
                : connected == true
                    ? AppLocalizations.of(context).save
                    : AppLocalizations.of(context).invalidUrl),
            onPressed: connected == true
                ? () {
                    GetIt.I<SharedPreferences>()
                      .setString('api', GetIt.I<AerisAPI>().baseRoute);
                    Provider.of<ActionCatalogueProvider>(context, listen: false).reloadCatalogue();
                    Navigator.of(context).pop();
                  }
                : null,
          )
        ]);
  }
}
