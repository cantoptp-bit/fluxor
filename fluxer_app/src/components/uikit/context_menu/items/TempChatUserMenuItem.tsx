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

import * as ModalActionCreators from '@app/actions/ModalActionCreators';
import {modal} from '@app/actions/ModalActionCreators';
import * as ToastActionCreators from '@app/actions/ToastActionCreators';
import {TempChatSetPasswordBeforeCreateModal} from '@app/components/modals/TempChatSetPasswordBeforeCreateModal';
import {MenuItem} from '@app/components/uikit/context_menu/MenuItem';
import type {UserRecord} from '@app/records/UserRecord';
import AuthenticationStore from '@app/stores/AuthenticationStore';
import {openTempChatForUser} from '@app/utils/TempChatUtils';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';
import {LockIcon} from '@phosphor-icons/react';

interface TempChatUserMenuItemProps {
	user: UserRecord;
	onClose: () => void;
}

export const TempChatUserMenuItem: React.FC<TempChatUserMenuItemProps> = observer(({user, onClose}) => {
	const {t} = useLingui();

	const handleStartTempChat = useCallback(() => {
		onClose();
		if (!AuthenticationStore.currentUserId) return;
		ModalActionCreators.push(
			modal(() => (
				<TempChatSetPasswordBeforeCreateModal
					user={user}
					onConfirm={async (password) => {
						ModalActionCreators.pop();
						try {
							await openTempChatForUser(user, password);
						} catch (err) {
							const message =
								err && typeof err === 'object' && 'message' in err
									? String((err as {message: string}).message)
									: t`Failed to open temp chat`;
							ToastActionCreators.createToast({type: 'error', children: message});
						}
					}}
					onCancel={() => ModalActionCreators.pop()}
				/>
			)),
		);
	}, [onClose, user, t]);

	return (
		<MenuItem icon={<LockIcon size={16} />} onClick={handleStartTempChat}>
			{t`Start temp chat`}
		</MenuItem>
	);
});
