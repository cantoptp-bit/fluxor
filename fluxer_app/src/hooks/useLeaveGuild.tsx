/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import * as GuildActionCreators from '@app/actions/GuildActionCreators';
import * as ModalActionCreators from '@app/actions/ModalActionCreators';
import {modal} from '@app/actions/ModalActionCreators';
import * as ToastActionCreators from '@app/actions/ToastActionCreators';
import {GenericErrorModal} from '@app/components/alerts/GenericErrorModal';
import {ConfirmModal} from '@app/components/modals/ConfirmModal';
import {Logger} from '@app/lib/Logger';
import GuildStore from '@app/stores/GuildStore';
import {Routes} from '@app/Routes';
import * as RouterUtils from '@app/utils/RouterUtils';
import {Trans, useLingui} from '@lingui/react/macro';
import {useCallback} from 'react';

const logger = new Logger('useLeaveGuild');

export const useLeaveGuild = () => {
	const {t} = useLingui();

	return useCallback(
		(guildId: string) => {
			const guild = GuildStore.getGuild(guildId);
			const isServer = guild?.guildType === 'server';
			const leaveLabel = isServer ? t`Leave Server` : t`Leave Community`;
			const failedTitle = isServer ? t`Failed to Leave Server` : t`Failed to Leave Community`;
			const failedMessage = isServer
				? t`We couldn't remove you from the server at this time.`
				: t`We couldn't remove you from the community at this time.`;
			const successMessage = isServer ? t`Left server` : t`Left community`;

			ModalActionCreators.push(
				modal(() => (
					<ConfirmModal
						title={leaveLabel}
						description={
							isServer ? (
								<Trans>Are you sure you want to leave this server? You will no longer be able to see any messages.</Trans>
							) : (
								<Trans>Are you sure you want to leave this community? You will no longer be able to see any messages.</Trans>
							)
						}
						primaryText={leaveLabel}
						primaryVariant="danger-primary"
						onPrimary={async () => {
							try {
								await GuildActionCreators.leave(guildId);
								RouterUtils.transitionTo(Routes.ME);
								ToastActionCreators.createToast({
									type: 'success',
									children: successMessage,
								});
							} catch (error) {
								logger.error('Failed to leave', error);
								window.setTimeout(() => {
									ModalActionCreators.push(
										modal(() => (
											<GenericErrorModal title={failedTitle} message={failedMessage} />
										)),
									);
								}, 0);
							}
						}}
					/>
				)),
			);
		},
		[t],
	);
};
