---
title: "Azure Resource Manager Templates"
slug: "azure-resource-manager-templates"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Syntax
- Syntax for ARM templates is well documented: https://azure.microsoft.com/en-us/documentation/articles/resource-group-authoring-templates/

## Create extension resource
*Extension Resources* in Azure are resources that extend other resources.

This template creates an Azure Key Vault as well as a DiagnosticSettings extension.

Things to note:
- The extension resource is created under the `resources` attribute of the parent resource
- It needs to have a `dependsOn` attribute referencing the parent resource (to prevent ARM from attempting to create the extension in parallel with the parent resource)


    {
      "$schema": "https://schema.management.azure.com/schemas/2015-01-01/deploymentTemplate.json#",
      "contentVersion": "1.0.0.0",
      "parameters": {
        "keyVaultName": {
          "type": "string",
          "metadata": {
            "description": "Name of the Vault"
          }
        },
        "tenantId": {
          "type": "string",
          "metadata": {
            "description": "Tenant ID of the directory associated with this key vault"
          }
        },
        "location": {
          "type": "string",
          "metadata": {
            "description": "Key Vault location"
          }
        },
        "storageAccountResourceGroup": {
          "type": "string",
          "metadata": {
            "description": "Resource Group of the storage account where key vault activities will be logged"
          }
        },
        "storageAccountName": {
          "type": "string",
          "metadata": {
            "description": "Name of the storage account where key vault activities will be logged.  Must be in same region as the key vault."
          }
        }
        },
      "resources": [
        {
          "type": "Microsoft.KeyVault/vaults",
          "name": "[parameters('keyVaultName')]",
          "apiVersion": "2015-06-01",
          "location": "[parameters('location')]",
          "properties": {
            "enabledForDeployment": "false",
            "enabledForDiskEncryption": "false",
            "enabledForTemplateDeployment": "false",
            "tenantId": "[variables('tenantId')]",
            "sku": {
              "name": "Standard",
              "family": "A"
            }
          },
          "resources": [
              {
          "type": "Microsoft.KeyVault/vaults/providers/diagnosticSettings",
          "name": "[concat(parameters('keyVaultName'), '/Microsoft.Insights/service')]",
          "apiVersion": "2015-07-01",
          "dependsOn": [
            "[concat('Microsoft.keyvault/vaults/', parameters('keyVaultName'))]"
          ],
          "properties": {
            "storageAccountId": "[resourceId(parameters('storageAccountResourceGroup'), 'Microsoft.Storage/storageAccounts', parameters('storageAccountName'))]",
            "logs": [{
                "category": "AuditEvent",
                "enabled": true,
                "retentionPolicy": {
                    "enabled": true,
                    "days": 90
                }
            }]
        }
        }]
        }
      ],
      "outputs": {
          "keyVaultUrl": {
              "type": "string",
              "value": "[reference(resourceId('Microsoft.KeyVault/vaults', parameters('keyVaultName'))).vaultUri]"
          }
      }
    }

